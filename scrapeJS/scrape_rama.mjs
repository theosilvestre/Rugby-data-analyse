import axios from "axios";             // retrieve static web html
import { chromium } from 'playwright'; // open web page inside a browser to manage js produced html
import * as cheerio from "cheerio";    // query in html
import fs from "node:fs"               // read/write files
import pLimit from 'p-limit';          // limit the number of parallelised querying streams
import {hashingFile, verifyHashThenWrite} from './scrape_annex.mjs';

const limit = pLimit(5);               // at most 5 threads

export async function sub_scrape_rama(url,writer,hashes,season) {
  console.log(url);
  /////// Fetch HTML page ///////
  const page = await axios.get(url, { headers: {"User-Agent": "Mozilla/5.0"} });
  /////// Query HTML page ///////
  var $$ = cheerio.load(page.data);
  const taway = $$('.a_idalgo_content_match_header_full_main_header_name_teams_visitor').text();
  const thome = $$('.a_idalgo_content_match_header_full_main_header_name_teams_local').text();
  const round = $$('.span_idalgo_content_match_header_full_details').text();
  const date = $$('.span_idalgo_content_match_header_full_date').text();
  const scoreaway = $$('.span_idalgo_content_match_header_full_main_header_center_score_visitor').text();
  const scorehome = $$('.span_idalgo_content_match_header_full_main_header_center_score_local').text();

  let minute = [];
  $$('.span_idalgo_content_match_action_part_minute').each(function(i,el){ minute[i] = $$(el).text(); });
  let action = [];
  $$('.span_idalgo_content_match_action_part_minute').next().each(function(i,el){ 
    action[i] = $$(el).attr('class').split('_').slice(-2); 
    if(action[i][0] === 'logo') action[i] = action[i][1];
    else if(action[i][1] === 'visitor' | action[i][1] === 'local') action[i] = action[i][0];
    else action[i] = action[i].join(' ');
  });
  let where = [];
  $$('.span_idalgo_content_match_action_part_minute').parent().each(function(i,el){ where[i] = $$(el).attr('class').split(' ')[1].split('_').pop(); });
  let player = [];
  $$('.a_idalgo_content_match_action_part_player').each(function(i,el){ 
    player[i] = [$$(el).text(),
                 $$(el).parent().next().find('.a_idalgo_content_match_action_part_detail').text()]
  });
  for (var i = 0; i < minute.length; i++){
    let arr;
    if(action[i]==='substitute'){
      arr = {minute:minute[i],type:action[i]+' out',home:thome,away:taway,where:where[i],round:round,player:player[i][0],date:date,season:season};
      verifyHashThenWrite(hashes,writer,arr);
      arr = {minute:minute[i],type:action[i]+' in',home:thome,away:taway,where:where[i],round:round,player:player[i][1],date:date,season:season};
      verifyHashThenWrite(hashes,writer,arr);
    } else {
      arr = {minute:minute[i],type:action[i],home:thome,away:taway,where:where[i],round:round,player:player[i],date:date,season:season};
      verifyHashThenWrite(hashes,writer,arr);
    }
  }
  return;
}

export async function scrape_rama(url,output) {
  /////// Hashing every rows of the provided output file ///////
  let hashes = await hashingFile(output);
  /////// Fetch HTML page ///////
  const response = await axios.get(url, { headers: {"User-Agent": "Mozilla/5.0"} });
  /////// Query HTML page ///////
  var $ = cheerio.load(response.data);
  //const season = [...new Set( $('.span_idalgo_content_calendar_cup_date_title_left').each(function(i,el){ $$(el).split(' ').at(-1); }) )].join('/');
  //console.log(season);
  let season = [];
  $('.span_idalgo_content_calendar_cup_date_title_left').each(function(i,el){ season[i] = $(el).text().split(' ').at(-1); });
  season = [...new Set( season )].join('/');

  let links = [];
  $('.a_idalgo_content_calendar_cup_date_match_score').each(function(i,el){ links[i] = $(el).attr('href'); });
  links = links.filter(function( el ) { return el !== undefined; });
  links.forEach((el, i, array) => { array[i] = url.split('/').slice(0,3).join('/')+el; });
  /////// Catching links for each matches ///////
  const writer = fs.createWriteStream(output+'.csv',{flags: 'a'});  // output csv file
  await Promise.all( links.map( url => limit(async () => await sub_scrape_rama(url,writer,hashes,season)) ) );
  writer.end();
  return;
}