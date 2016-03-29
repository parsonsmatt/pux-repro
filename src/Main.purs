module Main where

import Prelude (bind, const, map)
import Data.Maybe (fromMaybe)
import Data.Array (modifyAt, deleteAt, length, range, zip)
import Data.Tuple (uncurry)
import Data.Int

import Pux (renderToDOM, fromSimple, start)
import Pux.Html hiding (map, bind)
import Pux.Html.Events (onClick)

data Tree a = Tree a (Array (Tree a))

ex :: Tree String
ex =
    Tree "Hello"
        [ Tree "World" []
        , Tree "Adventure" 
            [ Tree "Time" []
            , Tree "Story" []
            ]
        ]

forEachIx :: forall a b. Array a -> (Int -> a -> b) -> Array b
forEachIx xs f = map (uncurry f) (zip (range 0 (length xs)) xs)

data Action = Child Int Action | Remove

view :: Tree String -> Html Action
view (Tree str subtrees) = ul # do
    li # do
        text str
        button ! onClick (const Remove) # text "X"
        div ## forEachIx subtrees \i tree ->
            forwardTo (Child i) (view tree)
  where
    bind = Pux.Html.bind

update :: forall a. Action -> Tree a -> Tree a
update (Child i Remove) (Tree t subtrees) = Tree t new
  where
    new = fromMaybe subtrees (deleteAt i subtrees)
update (Child i a) (Tree t subtrees) = Tree t new
  where
    new = fromMaybe subtrees (modifyAt i (update a) subtrees)
update Remove t = t

main = do
    app <- start
        { initialState: ex
        , update: fromSimple update
        , inputs: []
        , view: view
        }

    renderToDOM "#app" app.html
