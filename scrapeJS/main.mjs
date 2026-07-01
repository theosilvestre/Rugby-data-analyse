import {scrape_rama} from './scrape_rama.mjs';
import {sub_scrape_LNR_data,sub_scrape_LNR_stats,scrape_LNR} from './scrape_LNR.mjs';
import {sub_scrape_pass_data,sub_scrape_pass_stats,scrape_pass} from './scrape_pass.mjs';
import {launcher} from './scrape_annex.mjs';

const root_folder = '../website/public/data/'
const whichWebSite = 'rama' //['rama','LNR','pass']
if(whichWebSite === 'pass'){
  ////////// pass //////////
  const whichSeasons = ['2025/2026'];
  const union = "pro-d2"; //["pro-d2"]
  const calendarPath = "https://www.rugbypass.com/"+union+"/fixtures-results/";
  const dataOrStats = 'stats'; //['data','pass']
  const functions = {
    main: scrape_pass,
    data: sub_scrape_pass_data,
    stats: sub_scrape_pass_stats
  };
  launcher(calendarPath,root_folder+whichWebSite+'_'+union+'_'+dataOrStats,functions,whichSeasons,dataOrStats);
}
else if(whichWebSite === 'LNR'){
  ////////// LNR //////////
  const whichSeasons = [
    //'2026-2027', 
    '2025-2026',
    /*'2024-2025', 
    '2023-2024',
    '2022-2023', '2021-2022',
    '2020-2021', '2019-2020',
    '2018-2019', '2017-2018',
    '2016-2017', '2015-2016',
    '2014-2015', '2013-2014',
    '2012-2013', '2011-2012',
    '2010-2011', '2009-2010',
    '2008-2009', '2007-2008',
    '2006-2007', '2005-2006',
    '2004-2005'*/
  ];
  const union = "prod2"; //["prod2","top14"]
  const calendarPath = "https://"+union+".lnr.fr/calendrier-et-resultats/";
  const dataOrStats = 'stats';
  const functions = {
    main: scrape_LNR,
    data: sub_scrape_LNR_data,
    stats: sub_scrape_LNR_stats
  };
  launcher(calendarPath,root_folder+whichWebSite+'_'+union+'_'+dataOrStats,functions,whichSeasons,dataOrStats);
}
else if(whichWebSite === 'rama'){
  ////////// rama //////////
  const union = "pro-d2" //["pro-d2","top-14","premiership","challenge-cup","champions-cup"]
  const calendarPath = 'https://www.rugbyrama.fr/resultats/rugby/'+union+'/calendrier';
  let output = root_folder+whichWebSite+'_'+union;
  const functions = { main: scrape_rama };

  launcher(calendarPath,output,functions);
}