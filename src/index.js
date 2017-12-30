import 'bulma/css/bulma.css';
import 'font-awesome/css/font-awesome.min.css';
import RemoteStorage from 'remotestoragejs';
import Widget from 'remotestorage-widget';

import { Main } from './Main.elm';
import './css/main.css';

import registerServiceWorker from './js/registerServiceWorker';
import booksModule from './js/rs-books-module'; 

let booksStorage = "books002";
let rs = new RemoteStorage({
  modules: [booksModule(booksStorage)]
});
rs.access.claim(booksStorage, 'rw');
let widget = new Widget(rs);
widget.attach("widget");
rs[booksStorage].init();

let startElm = (function(startingBooks) {

  const elm = Main.embed(document.getElementById('root'), startingBooks);

  elm.ports.setStorage.subscribe(strBooks => {
    console.log("setStorage called with: ", strBooks);
  });

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

  rs.on('disconnected', function () {
    console.log("Disconnected. Reloading page...");
    location.reload();
  });

  registerServiceWorker();
})

rs.on('ready', function () {
  console.log("rs ready.")
  rs[booksStorage].list()
    .then(listing => {
      startElm(JSON.stringify(listing));
    });
});
