import 'bulma/css/bulma.css';
import 'bulma-extensions/bulma-checkradio/bulma-checkradio.min.css';
import 'font-awesome/css/font-awesome.min.css';
import RemoteStorage from 'remotestoragejs';
import Widget from 'remotestorage-widget';

import { Main } from './Main.elm';
import './css/main.css';

import registerServiceWorker from './js/registerServiceWorker';
import booksModule from './js/rs-books-module'; 

let booksStorage = "transaccion";
let rs = new RemoteStorage({
  modules: [booksModule(booksStorage)]
});
rs.access.claim(booksStorage, 'rw');
let widget = new Widget(rs);
widget.attach("widget");
rs[booksStorage].init();

let startElm = (function(startingBooks) {

  const elm = Main.embed(document.getElementById('root'), startingBooks);

  elm.ports.deleteBook.subscribe(bookId => {
    rs[booksStorage].remove(bookId)
      .then(() => {
        console.log(`book ${bookId} deleted.`);
      });
  })

  elm.ports.saveBook.subscribe(strBook => {
    let objBook = JSON.parse(strBook);
    rs[booksStorage].add(objBook)
      .then(() => {
        console.log(`book ${objBook.id} saved.`)
      });
  })

  rs[booksStorage].on('change', function (event) {
    if (event.newValue && (!event.oldValue)) {
      // console.log('Change from ' + event.origin + ' (add)', event);
    }
    else if ((!event.newValue) && event.oldValue) {
      // console.log('Change from ' + event.origin + ' (remove)', event);
    }
    else if (event.newValue && event.oldValue) {
      // console.log('Change from ' + event.origin + ' (change)', event);
    }
  });

  registerServiceWorker();
});

rs.on('ready', function () {
  console.log("rs ready.");
  rs[booksStorage].list()
    .then(listing => {
      startElm(JSON.stringify(listing));
    });
});

rs.on('disconnected', function () {
  console.log("rs disconnected.");
  location.reload();
});

rs.on('connected', function () {
  console.log("rs connected.");
});