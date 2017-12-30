import 'bulma/css/bulma.css';
import 'font-awesome/css/font-awesome.min.css';
import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

import RemoteStorage from 'remotestoragejs';
import Widget from 'remotestorage-widget';

// BOOKS MODULE //

const bookType = {
  "type": "object",
  "properties": {
    "id": { 
      "type": "string" 
    },
    "name": { 
      "type": "string" 
    },
    "expenseCategories": {
      "type": "array",
      "items": "string"
    },
    "earningCategories": {
      "type": "array",
      "items": "string"
    },
    "transactions": {
      "type": "object",
      "additionalProperties": {
        "type": "object",
        "properties": {
          "id": {
            "type": "string" 
          },
          "price": {
            "type": "number",
            "format": "double"
          },
          "category": { 
            "type": "string" 
          },
          "description": { 
            "type": "string" 
          }
        }
      }
    }
  }
}

/////////////////

let rs = new RemoteStorage();
let storageFolder = "test-storage-016";
rs.access.claim(storageFolder, 'rw');
const booksClient = rs.scope(`/${storageFolder}/`);

booksClient.declareType('bookType', bookType);
booksClient.cache('');

let widget = new Widget(rs);
widget.attach("widget");

// booksClient.getAll('')
//   .then(arr => {
//     let objBooks = {};
//     if (arr.length > 0) {
//       console.log("Something is stored! Printing them below.")
//       for (let item in arr) {
//         console.log(item);
//         objBooks[item['id']] = item;
//       };
//       console.log("This is the final value of objBooks: ", objBooks);
//     } else {
//       console.log("sorry, there is nothing in your storage.");
//       for (let key in defaultBooks) {
//         booksClient.storeObject('bookType', key, defaultBooks[key]);
//       }
//       objBooks = defaultBooks;
//     };
//     startElm(JSON.stringify(objBooks));
//   });

let listing = [];
let savedBooks = {};

rs.on('ready', function () {
  console.log("hmmm, ready.");
  booksClient.getListing('')
    .then(listing => {
      let booksPromises = [];
      for (let item in listing) {
        console.log(item);
        booksPromises.push(booksClient.getObject(item)
          .then(obj => [obj.id, obj]));
      };
      Promise.all(booksPromises).then(books => {
        let startingBooks = {};

        if (books.length > 0) {
          let storedBooks = {}
          for (let i in books) {

            let pair = books[i];
            let id = pair[0];
            let obj = pair[1];
            storedBooks[id] = obj;
          };
          startingBooks = JSON.stringify(storedBooks);
        } else {
          for (let key in defaultBooks) {
            booksClient.storeObject('bookType', key, defaultBooks[key]);
          };
          startingBooks = JSON.stringify(defaultBooks);
        };
        // let startingBooks = JSON.stringify(defaultBooks);
        startElm(startingBooks);
      })
    });
})

rs.on('disconnected', function () {
  console.log("rs is disconnected.");
});

rs.on('connected', function () {
  console.log("You are connected.");
});

rs.on('error', () => {
  console.log("ERRRORRRRR!, permit me to reload.");
  location.reload();
})

booksClient.on('change', function (event) {
  console.log("THERE IS A CHANGE JUST NOW!", event);
});


let startElm = (function(startingBooks) {

  const elm = Main.embed(document.getElementById('root'), startingBooks);

  elm.ports.setStorage.subscribe(strBooks => {
    let objBooks = JSON.parse(strBooks);
    console.log(strBooks);
    for (let key in objBooks) {
      console.log(objBooks[key]);
      booksClient.storeObject('bookType', key, objBooks[key])
        .then(() => {console.log("saved successfully.")});
    };
    booksClient.getListing('')
      .then(listing => {
        for (let key in listing) {
          console.log(key);
        };
      })
  });

  elm.ports.deleteBook.subscribe(bookId => {
    booksClient.remove(bookId)
      .then(() => {
        console.log("Successfully removed! But really?");
      })
  })

  elm.ports.saveBook.subscribe(strBook => {
    let objBook = JSON.parse(strBook);
    if (objBook.id != "dummy") {
      booksClient.storeObject('bookType', objBook.id, objBook)
        .then(() => {
          console.log("saved successfully! But did it?");
        })
    }
  })

  registerServiceWorker();
})

/// constants

const transaction00 = {
  "id" : "00",
  "price" : 13.4,
  "category" : "Food",
  "description" : "quality food"
}

const transaction01 = {
  "id": "01",
  "price": 1.4,
  "category": "Misc",
  "description": "toothpaste"
}

const defaultTransactions = {
  "00" : transaction00,
  "01" : transaction01
}

const book00 = {
  "id" : "00",
  "name" : "Personal",
  "expenseCategories": ["Uncategorized", "Food", "Rent", "Transpo", "Leisure", "Misc", "Subscription", "Medical", "Unexpected"],
  "earningCategories": ["Uncategorized", "Salary", "Bonus", "Gift", "Reimbursement", "Sideline", "Unexpected"],
  "transactions" : defaultTransactions
}

const book01 = {
  "id": "01",
  "name": "Business",
  "expenseCategories": ["Uncategorized", "Food", "Rent", "Transpo", "Leisure", "Misc", "Subscription", "Medical", "Unexpected"],
  "earningCategories": ["Uncategorized", "Salary", "Bonus", "Gift", "Reimbursement", "Sideline", "Unexpected"],
  "transactions": defaultTransactions
}

const defaultBooks = {
  "00" : book00,
  "01" : book01
}
