import axios from "axios";             // retrieve static web html
import { chromium } from 'playwright'; // open web page inside a browser to manage js produced html
import * as cheerio from "cheerio";    // query in html
import fs from "node:fs"               // read/write files
import pLimit from 'p-limit';          // limit the number of parallelised querying streams
import {hashingFile, verifyHashThenWrite} from './scrape_annex.mjs';

const limit = pLimit(5);               // at most 5 threads

async function fetchSeasonsRounds(calendarPath) {
  const page = await axios.get(calendarPath, { headers: {"User-Agent": "Mozilla/5.0"} });
  var $ = cheerio.load(page.data);
  const el = $('filters-fixtures').attr(':filter-list');
  let tmp;
  tmp = JSON.parse(el)['seasons']
  let ids = Object.fromEntries(JSON.parse(el)['seasons'].map(function (el){ return [el['name'],el['id']]; }));
  let seasons = JSON.parse(el)['seasons'].map(function (el){ return el['name'];}) ;
  let rounds = Object.fromEntries( seasons.map(function (season){ return [season,JSON.parse(el)['weeks'][ids[season]].map(function (el){ return el['slug']; })]; }) );
  return rounds;
}

export async function sub_scrape_LNR_data(url,writer,hashes,season,round){
  const page = await axios.get(url, { headers: {"User-Agent": "Mozilla/5.0"} });
  var $ = cheerio.load(page.data);
  const taway = $('.match-header-club__wrapper--right').find('.match-header-club__title').text();
  const thome = $('.match-header-club__wrapper--left').find('.match-header-club__title').text();
  const date = $('.match-header__season-day').text().split(" - ")[2];
  const el = $('vertical-timeline').each(function (i,el){ $(el).attr(':items');  });

  let minute;
  let tmp;
  let arr;
  tmp = JSON.parse(el['0']['attribs'][':items'])// remplaçant : 1
  for(var i = 0; i < tmp.length; i++){
    if(tmp[i]['additionalMinute'] > 0) minute = tmp[i]['minute']+"'+"+tmp[i]['additionalMinute']
    else minute = tmp[i]['minute']+"'"
    arr = {minute:minute,type:tmp[i]['slugSubType'],home:thome,away:taway,where:tmp[i]['club'],round:round,player:(tmp[i]['player']['firstName']+' '+tmp[i]['player']['lastName']).toLowerCase(),date:date,season:season};
    verifyHashThenWrite(hashes,writer,arr);
    if('conversionPlayer' in tmp[i]){
      arr = {minute:minute,type:'conversion',home:thome,away:taway,where:tmp[i]['club'],round:round,player:(tmp[i]['conversionPlayer']['firstName']+' '+tmp[i]['conversionPlayer']['lastName']).toLowerCase(),date:date,season:season};
      verifyHashThenWrite(hashes,writer,arr);
    }
  }
  let inout = ['in','out'];
  tmp = JSON.parse(el['1']['attribs'][':items'])// remplaçant : 1
  for(var i = 0; i < tmp.length; i++){
    for(var j = 0; j < inout.length; j++){
      if(tmp[i][inout[j]] !== null){
        arr = {minute:tmp[i]['minute']+"'",type:'substitute '+inout[j],home:thome,away:taway,where:tmp[i]['club'],round:round,player:(tmp[i][inout[j]]['firstName']+' '+tmp[i][inout[j]]['lastName']).toLowerCase(),date:date,season:season};
        verifyHashThenWrite(hashes,writer,arr);
      }
    }
  }

  return;
}

export async function sub_scrape_LNR_stats(url,writer,hashes,season,round){
  const page = await axios.get(url+'/statistiques-du-match', { headers: {"User-Agent": "Mozilla/5.0"} });
  var $$ = cheerio.load(page.data);
  const taway = $$('.match-header-club__wrapper--right').find('.match-header-club__title').text();
  const thome = $$('.match-header-club__wrapper--left').find('.match-header-club__title').text();
  const date = $$('.match-header__season-day').text().split(" - ")[2];
  const scoreaway = $$('.match-header__title').text().split(" - ")[0].trim();
  const scorehome = $$('.match-header__title').text().split(" - ")[1].trim();

  let action = [];
  let statsHome = [];
  let statsAway = [];
  let toncar = [];
  let color = [];
  $$('.stats-bar__val--left').each(function(i,el){ statsHome[i] = $$(el).text().trim(); });
  $$('.stats-bar__val--right').each(function(i,el){ statsAway[i] = $$(el).text().trim(); });
  $$('.stats-bar__title').each(function(i,el){ action[i] = $$(el).text(); });
  $$('.stats-cards-fault__card').each(function(i,el){ toncar[i] = $$(el).text(); });
  $$('.stats-cards-fault__label').each(function(i,el){ color[i] = $$(el).text(); });
  color = color.slice(0,3);

  let arr;
  for (var i = 0; i < action.length; i++){
    arr = {type:action[i],home:thome,away:taway,statsHome:statsHome[i],statsAway:statsAway[i],round:round,date:date,season:season};
    verifyHashThenWrite(hashes,writer,arr);
  }
  for (var i = 0; i < color.length; i++){
    arr = {type:color[i],home:thome,away:taway,statsHome:toncar[i],statsAway:toncar[i+3],round:round,date:date,season:season};
    verifyHashThenWrite(hashes,writer,arr);
  }
  arr = {type:"score",home:thome,away:taway,statsHome:scorehome,statsAway:scoreaway,round:round,date:date,season:season};
  verifyHashThenWrite(hashes,writer,arr);

  return;
}


export async function scrape_LNR(calendarPath,output,whichSeasons,sub_scrape){
  /////// Hashing every rows of the provided output file ///////
  let hashes = await hashingFile(output);
  /////// Fetch seasons and rounds ///////
  const fullCalendar = await fetchSeasonsRounds(calendarPath);
  var seasons = Object.keys(fullCalendar);
  if(!whichSeasons.every(el => seasons.includes(el))) {
    console.log('Provided seasons through whichSeasons are out of scope. Must include seasons in those : '+seasons.join(' '));
    return;
  } else { seasons = whichSeasons; }
  console.log("Seasons: "+seasons.join(', '));

  console.log("Fetching links for every rounds of selected seasons...")
  const writer = fs.createWriteStream(output+'.csv',{flags: 'a'});  // output csv file
  
  const limitLinks = pLimit(10);  
  for (const season of seasons) {
    console.log(season);
    await Promise.all(
      fullCalendar[season].map(async round => 
        limit(async () => {
          process.stdout.write(round+' ');
          const page = await axios.get(calendarPath+season+'/'+round.replace(/ /g,'-'), { headers: {"User-Agent": "Mozilla/5.0"} });
          var $ = cheerio.load(page.data);
          const roundLink = $('.match-line__score').map((_, el) => $(el).attr('href')).get();
          await Promise.all( roundLink.map(async link => limitLinks(async () => await sub_scrape(link,writer,hashes,season,round)) ) )
        })
      )
    )
  }
  writer.end();

  return;
}