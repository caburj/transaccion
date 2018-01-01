## Vocabularies

1. expense - expenditure, loss
1. earning - profit, gain

## Currently working



## TODO

- remove delete in bookCard
  - or if not, ask for confirmation to delete.
  - use bulma model for delete confirmation
- input price -> is-danger if not valid
- the homepage
  - record your transactions to properly monitor your money
  - work with privacy using your own 5apps account
  - make a profit
  - or or or
  - record your transactions
  - in multiple organizational books
  - save locally or sync online
- options to display transactions by month, year and all
- github-like timeline for transactions
  - put below categories
- summary as part of model
- documentation
- post to twitter and facebook with blog-entry
- chart above categories
- better summary
  - checkbox for including/excluding expenses/earnings
- display transactions -> this is done but only simple
  - format: Date (Month above, day below), Price Category and description (price above, category beside, description below)
  - is-danger if expense, is-link if earning
  - remove negative in display
- pages for monthly transactions
  - use bulma pagination
- summary by month
  - month | total expense | total earning | balance
- summary for each category in the displayed transactions
  - separate expenses from earnings
- save to csv
- User can defined his own defaultCategories.


## Implemented

- user can add categories
- add created and lastEdited properties for book and transaction
- set max length for inputcategory
- input boxes and dropdowns
- add transactions
- delete transaction
- summary by category - not yet final


## Notes

1. put assets in `public` folder.
1. access public folder in the `src` code using `%PUBLIC_URL%`

## Coding process

1. `elm-app start` to start dev server
1. edit `src` code
1. `elm-app build` for production
1. `gh-pages -d build` to deploy to github.

## References

1. Readme of [Create Elm App](https://github.com/halfzebra/create-elm-app/blob/master/template/README.md)