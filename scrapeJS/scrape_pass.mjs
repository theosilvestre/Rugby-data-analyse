import axios from "axios";             // retrieve static web html
import FormData from 'form-data';
import { chromium } from 'playwright'; // open web page inside a browser to manage js produced html
import * as cheerio from "cheerio";    // query in html
import fs from "node:fs"               // read/write files
import pLimit from 'p-limit';          // limit the number of parallelised querying streams
import {hashingFile, verifyHashThenWrite} from './scrape_annex.mjs';

const limit = pLimit(5);               // at most 5 threads

export async function sub_scrape_pass_data(url,writer,hashes,season){
  console.log(url);
  try {
    const page = await axios.get(url, { headers: {"User-Agent": "Mozilla/5.0"} });
    var $$ = cheerio.load(page.data);
    let tmp = [];
    $$('.team .team-name').each(function(i,el){ tmp[i] =  $$(el).text(); });
    const taway = tmp[1];
    const thome = tmp[0];
    $$('.match-details .title').each(function(i,el){ tmp[i] =  $$(el).text(); });
    const round = tmp[1].trim();
    const date = tmp[2].split(',')[0].trim();
    const scoreaway = $$('.away-score').text();
    const scorehome = $$('.home-score').text();

    let action, minute, player, transfo, arr;
    const where = ['home','away'];
    for(var j = 0; j < 2; j++){
      action = [];
      minute = [];
      player = [];
      $$('.key-events-container .side.'+where[j]).each(function(i,el){
        if(where[j]==='home') tmp = $$(el).parent().next().find('.icon-image').attr('class');
        if(where[j]==='away') tmp = $$(el).parent().prev().find('.icon-image').attr('class');
        if(typeof(tmp) !== "undefined") action.push(tmp.split(' ')[1]);
        tmp = $$(el).find('.score .time').text();
        if(tmp !== '') minute.push(tmp);
        tmp = $$(el).find('.name').text().toLowerCase().trim();
        if(tmp !== '') player.push(tmp);
      });
      for (var i = 0; i < minute.length; i++){
        arr = {minute:minute[i],type:action[i],home:thome,away:taway,where:where[j],round:round,player:player[i],date:date,season:season};
        verifyHashThenWrite(hashes,writer,arr);
      }
    }

    tmp = url.split('?g=');
    const substituteshtml = await axios.get(tmp[0]+'teams/?g='+tmp[1], { headers: {"User-Agent": "Mozilla/5.0"} });
    var $$$ = cheerio.load(substituteshtml.data);
    for(var j = 0; j < 2; j++){
      action = [];
      minute = [];
      player = [];
      $$$('[data-id='+where[j]+'-team] .players .player').each(function(i,el){
        action[i] = $$$(el).find('.sub').children().map(function(k,el){ return 'substitute '+$$$(el).attr('class'); }).get();
        minute[i] = $$$(el).find('.sub').children().map(function(k,el){ return $$$(el).text().trim();  }).get();
        player[i] = $$$(el).find('.name').text().toLowerCase().trim();
      });

      for (var i = 0; i < action.length; i++){
        for (var k = 0; k < action[i].length; k++){
          if(action[i].length > 0){
            arr = {minute:minute[i][k],type:action[i][k],home:thome,away:taway,where:where[j],round:round,player:player[i],date:date,season:season};
            verifyHashThenWrite(hashes,writer,arr);
          }
        }
      }
    }
  } catch (err) {
    if (err.response?.status === 404) {
        console.log(`Broken RugbyPass link: ${url}`);
        return null;
    }
    throw err;
  }
  
  return;
}

export async function sub_scrape_pass_stats(url,writer,hashes,season){
  console.log(url);
  try {
    const page = await axios.get(url, { headers: {"User-Agent": "Mozilla/5.0"} });
    var $$ = cheerio.load(page.data);
    let tmp = [];
    $$('.team .team-name').each(function(i,el){ tmp[i] =  $$(el).text(); });
    const taway = tmp[1];
    const thome = tmp[0];
    $$('.match-details .title').each(function(i,el){ tmp[i] =  $$(el).text(); });
    const round = tmp[1].trim();
    const date = tmp[2].split(',')[0].trim();

    let action, statsHome, statsAway, arr;
    action = ['score'];
    statsHome = [$$('.home-score').text()];
    statsAway = [$$('.away-score').text()];
    $$('.worm-details .row').each(function(i,el){
      if(i!== 0){
        statsHome.push($$(el).find('.left').text());
        action.push($$(el).find('.mid').text());
        statsAway.push($$(el).find('.right').text());
      }
    })

    const scripts = $$("script").map((i, el) => $$(el).html()).get();
    for(const radar of ['AttackRadar','DefenceRadar']){
      for (const script of scripts) {
        if (!script.includes("RadarChart")) continue;
        const match = script.match( new RegExp(`RadarChart\\("${radar}",\\s*(\\[[\\s\\S]*?\\])\\s*,`) );
        if (match) {
          const rawArray = match[1];
          let radarData = JSON.parse(rawArray);
          for(var i=0;i<radarData.length;i++){
            statsHome.push(radarData[i]['homeTeam']['value']);
            action.push(radarData[i]['homeTeam']['label']);
            statsAway.push(radarData[i]['awayTeam']['value']);
          }
          break;
        }
      }
    }

    $$('.graph').each(function(i,el){
      statsHome.push($$(el).find('.home .label').text());
      action.push($$(el).first().contents().first().text().trim());
      statsAway.push($$(el).find('.away .label').text());
    })

    let ol = $$('.rucks-won');
    statsHome.push($$(ol).find('.home').text().trim());
    action.push('rucks won');
    statsAway.push($$(ol).find('.away').text().trim());

    tmp = url.split('?g=');
    const statshtml = await axios.get(tmp[0]+'stats/?g='+tmp[1], { headers: {"User-Agent": "Mozilla/5.0"} });
    var $$$ = cheerio.load(statshtml.data);

    tmp = $$$('.stat').map((i,el) => 
      [$$$(el).find('div').map((j,ol) => {
        let tp = $$$(ol).text();
        if(tp !== '') return tp;
      }).get()] 
    ).get()

    for(var i = 0; i < tmp.length; i++){  
      if(typeof(tmp[i][0]) !== 'undefined'){
        statsHome.push(tmp[i][0]);
        action.push(tmp[i][1]);
        statsAway.push(tmp[i][2]);
      }
    }

    let tmp2, tmp3;
    tmp2 = $$$('[id=territory_canvas_percents]').children().map((i,el) => $$$(el).text() ).get();
    action.push('territory 0-22','territory 22-50','territory 50-22','territory 22-0');
    for(var i = 0; i < tmp2.length; i++){
      statsHome.push(tmp2[i]);
      statsAway.push(tmp2[i]);
    }
    action.push('possession 0-22','possession 22-50','possession 50-22','possession 22-0');
    tmp2 = $$$('[id=home_poss_canvas_percents]').children().map((i,el) => $$$(el).text() ).get() ;
    tmp3 = $$$('[id=away_poss_canvas_percents]').children().map((i,el) => $$$(el).text() ).get();
    for(var i = 0; i < tmp2.length; i++){
      statsHome.push(tmp2[i]);
      statsAway.push(tmp3[i]);
    }
    for (var i = 0; i < action.length; i++){
      for (var k = 0; k < action[i].length; k++){
        if(action[i].length > 0){
          arr = {type:action[i],home:thome,away:taway,statsHome:statsHome[i],statsAway:statsAway[i],round:round,date:date,season:season};
          verifyHashThenWrite(hashes,writer,arr);
        }
      }
    }
  } catch (err) {
    if (err.response?.status === 404) {
        console.log(`Broken RugbyPass link: ${url}`);
        return null;
    }
    throw err;
  }
  
  return;
}

async function getFixtures(season) {
  const form = new FormData();
  form.append('action', 'filter-fixtures');
  form.append('season', season);
  form.append('team', '0');
  form.append('comp', '0');
  form.append('isContent', '1');
  const res = await axios.post(
    'https://www.rugbypass.com/pro-d2/fixtures-results',
    form,{  headers: {
                      ...form.getHeaders(),
                      'user-agent': 'Mozilla/5.0',
                      'referer': 'https://www.rugbypass.com/pro-d2/fixtures-results'
                      }
          }
  );
  return res.data;
}

export async function scrape_pass(calendarPath,output,whichSeasons,sub_scrape){
  ////// Hashing every rows of the provided output file ///////
  let hashes = await hashingFile(output);
  ////// Fetch seasons and rounds //////
  const page = await axios.get(calendarPath, { headers: {"User-Agent": "Mozilla/5.0"} });
  var $ = cheerio.load(page.data);
  const match = $.html().match(/seasons:\s*(\[[\s\S]*?\])\s*,\s*loading:/);
  const blockSeasons = Function("return " + match[1])();   
  let seasons = blockSeasons.map(function(s) { return [s.label,s.season];  });
  const seasonLabel = seasons.map(function(s) { return s[0]; });

  if(!whichSeasons.every(el => seasonLabel.includes(el))) {
    console.log('Provided seasons through whichSeasons are out of scope. Must include seasons in those : '+seasonLabel.join(' '));
    return;
  } else { 
    seasons = whichSeasons.map(function(s){ return [s,Number(s.split('/')[1])]; });
  }
  console.log("Seasons: "+seasons.map(function(s) { return s[0]; }));

  const writer = fs.createWriteStream(output+'.csv',{flags: 'a'});  // output csv file
  const limitLinks = pLimit(10);  
  let tmp1;
  let tmp2;
  let res;
  for(var i = 0; i < seasons.length; i++){
    console.log(seasons[i][0]);
    res = await getFixtures(seasons[i][1]);
    await Promise.all(
      res['currentGameDays'].map(async round => 
        limit(async () => {
          await Promise.all(
            round['tournaments'][0]['games'].map(async link => 
              limitLinks(async () => {
                await sub_scrape(link['url'],writer,hashes,seasons[i][0])
              }) 
            ) 
          );
        })
      )
    );
  }
  await writer.close();

}