import 'bulma/css/bulma.css';
import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

import RemoteStorage from 'remotestoragejs';
import Widget from 'remotestorage-widget';


const folder = "/test-storage-003/";
const remoteStorage = new RemoteStorage();
remoteStorage.access.claim(folder.slice(1, folder.length - 1), 'rw');
// remoteStorage.onChange(folder, event => {console.log("change event in remoteStorage: ", event)});
// remoteStorage.cache.enable(folder);
const client = remoteStorage.scope(folder);

const bookType = {
  "type": "object",
  "properties": {
    "id" : "int",
    "name" : "string",
    "transactions" : {
      "type": "array",
      "items" : transactionType
    }
}}

const transactionType = {
  "type": "object",
  "properties": {
    "price" : "float",
    "category" : "string",
    "description": "string"
  }
}

client.declareType('books', {
  "type": "array",
  "items": bookType
})

const widget = new Widget(remoteStorage);
widget.attach("widget");

let storedBooks = null;
//
// client.getListing('')
//   .then(listing => {
//     console.log(listing);
//   })
client.getObject('books')
  .then(books => {
    if (books) {
        storedBooks = books;
    } else {
        storedBooks = [];
    };
    // console.log("storedBooks from client = ", storedBooks)
    elmStart();
  });

let elmStart = (function() {
  // widget.close();
  // console.log("storedBooks used by elm = ", storedBooks);
  const elm = Main.embed(document.getElementById('root'), storedBooks);

  elm.ports.setStorage.subscribe(books => {
    // console.log(books);
    client.storeObject('books', 'books', books)
      .then("Books saved successfully")
      .catch(err => console.log(err));
  });

  registerServiceWorker();
})
