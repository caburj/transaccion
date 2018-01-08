## Vocabularies

1. expense - expenditure, loss
1. earning - profit, gain

## Currently working

- summary as part of model
- summary of ALL transactions by month
  - month | total expense | total earning | balance
- documentation
- credits to the used open-sourced apps
- LOGO!!!

## TODO

- 100% RESPONSIVE!!
- add percentage in the summary
- how about previous 30 days of view?
- github-like timeline for transactions
  - put below categories
- pages for monthly transactions
  - use bulma pagination
- save to csv
- edit mode
- User can defined his own defaultCategories.


## Implemented

- user can add categories
- add created and lastEdited properties for book and transaction
- set max length for inputcategory
- input boxes and dropdowns
- add transactions
- delete transaction
- summary by category - not yet final
- deleteBookConfirmation
- input price -> is-danger if not valid
- display transactions -> this is done but only simple
  - format: Date (Month above, day below), Price Category and description (price above, category beside, description below)
  - is-danger if expense, is-link if earning
  - remove negative in display
- options to display transactions by month, year and all
- fix page not found error
- summary of displayed transactions by category
  - separate expenses from earnings
- chart above categories
- use better blue and better red for color of prices
- fix the location of the current balance
- fix the location of the delete button
- put tooltips in the input fields
- post to facebook with blog-entry
- post to twitter
- query
- summary of current month in bookCard
- the homepage
  - record your transactions to properly monitor your money
  - work with privacy using your own 5apps account
  - make a profit
  - or or or
  - record your transactions
  - in multiple organizational books
  - save locally or sync online
- transactionsToDisplay defaults to current month
- confirm delete


## Issues

### List
- add currency
- visibility of earning and expense options

### Fixed
- tab press at descriptionInput to focus on priceInput

## Important Notes

1. put assets in `public` folder.
1. access public folder in the `src` code using `%PUBLIC_URL%`

## Coding process

1. `elm-app start` to start dev server
1. edit `src` code
1. `elm-app build` for production
1. `gh-pages -d build` to deploy to github.

## References

1. Readme of [Create Elm App](https://github.com/halfzebra/create-elm-app/blob/master/template/README.md)
