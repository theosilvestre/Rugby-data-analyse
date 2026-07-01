import fs from "node:fs"               // read/write files
import xxhash from 'xxhash-wasm';      // hashing algo, not crypto
import csv from 'csv-parser';
import readline from 'readline';
import fsPromises from 'fs/promises';

let h64;

async function getLine(pathToFile,firstLast) {
  const readable = fs.createReadStream(pathToFile);
  const reader = readline.createInterface({ input: readable });
  let line;

  if(firstLast === 'first'){
    line = await new Promise((resolve) => {
      reader.on('line', (line) => {
        reader.close();
        resolve(line);
      });
    });
  } else if(firstLast === "last") {
    //for await (const l of reader) line = l;
    line = await new Promise((resolve) => {
      let l;
      reader.on('line', (currentLine) => { l = currentLine; });
      reader.on('close', () => { resolve(l); });
    });
  }
  readable.close();
  return line;
}


async function hasTrailingEmptyRow(pathToFile) {
  const fh = await fsPromises.open(pathToFile, 'r');
  try {
    const stats = await fh.stat();

    if (stats.size === 0) return true;

    const buf = Buffer.alloc(1);
    await fh.read(buf, 0, 1, stats.size - 1);

    return buf[0] === 10; // '\n'
  } finally {
    await fh.close();
  }
}

async function initHash() {
    const xx = await xxhash();
    h64 = xx.h64;
}
function serialize(row) {
    const keys = Object.keys(row);
    return keys.map(k => `${k}=${row[k]}`).join('\x1F');
}

export async function hashingFile(output) {
  /////// Hashing the given CSV to find deduplicated rows ///////
  const hashes = new Set();
  await initHash();
  return new Promise((resolve, reject) => {
    fs.createReadStream(output + ".csv")
      .pipe(csv({ separator: ';' }))
      .on("data", row => {
        hashes.add(h64(serialize(row)));
      })
      .on("end", () => resolve(hashes))
      .on("error", reject);
  });
}

export async function verifyHashThenWrite(hashes,writer,arr) {
  let arrhash = h64(serialize(arr));
  if(!hashes.has(arrhash)){
    hashes.add(arrhash);
    writer.write(Object.values(arr).join(';')+'\n');
  }
  return;
}


export async function launcher(calendarPath,output,functions,whichSeasons='',dataOrStats='data'){
  let cols;
  if(dataOrStats === 'data'){ cols = ["minute","type","home","away","where","round","player","date","season"];
  } else if(dataOrStats === 'stats'){ cols = ["type","home","away","statsHome","statsAway","round","date","season"];
  } else { console.log('dataOrStats must be either data or stats');
  }
  /////// Checking if output exists ///////
  await fs.promises.access(output+'.csv', fs.constants.F_OK).then(async () => {
    /////// Checking if columns' name are good ///////
    const tmpFirstLine = await getLine(output+'.csv',"first"); 
    if(tmpFirstLine !== cols.join(";")){
      console.log("Wrong columns in output file. Current first line is "+tmpFirstLine+" . Must be "+dataCols.join(";")+"\n.");
    } else { 
      console.log("Output file provided. The program will append rows to it.");
      /////// Checking if the last row is empty, to correctly append new ones ///////
      if(await hasTrailingEmptyRow(output+'.csv') == false){
        const writer = fs.createWriteStream(output+'.csv',{flags: 'a'});
        writer.write('\n');
        writer.end();
      }
    }
  }).catch(async () => {
    /////// The file does not exist, so it is created ///////
    fs.writeFile(output+'.csv', cols.join(";")+'\n', 'utf8', async function (err) {
      if (err) { console.log('Output file has not been created. Some error occured - file either not saved or corrupted file saved.');
      } else{  console.log('Output file has been created.'); 
      }
    });
  })
  /////// Launching the scraping ///////
  if (functions['main'].length === 2) { await functions['main'](calendarPath,output);
  } else if (functions['main'].length >= 3) { await functions['main'](calendarPath,output,whichSeasons,functions[dataOrStats]); 
  } 

  return;
}