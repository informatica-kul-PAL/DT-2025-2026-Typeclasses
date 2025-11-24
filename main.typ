#import "@preview/frame-it:1.2.0": *

#let thema = blue
#set page(paper: "presentation-16-9")
#set text(font: "IBM Plex Sans", 18pt)
#set raw(lang: "hs")

#show link: it => underline(text(blue, it))
#show raw: it => {
  set text(font: "Iosevka NFM", 1.2em)
  box(
    fill: thema.lighten(80%).desaturate(90%), 
    outset: (x: 0pt, y: 0.3em), 
    inset: (x: 0.35em, y: 0pt), 
    radius: 0.15em, 
    it
  )
}
#show: frame-style(styles.thmbox)

#let definition = frame("Definitie", blue)

#page(fill: thema.lighten(80%))[
  #set text(font: "Funnel Display", 24pt)
  #show strong: set text(thema)

  #rect(
    stroke: (left: 4pt + thema),
    inset: (x: 12pt),
    [
      #block(below: 10pt, text(12pt)[24 november 2025])
      #block(below: 20pt, text(16pt)[PAL-sessie Declaratieve Talen])
    ]
  )
  #v(1fr)
  #block(text(64pt, weight: "light")[Sessie II: *typeclasses*])
]

#page(align(center + horizon, text(32pt)[
  Zijn er vragen vanuit de oefenzittingen?
]))

#page[
  = I. Typeclasses
  - Wat zijn typeclasses?
  - Hoe pas je een typeclass toe?
  - Nuttige typeclasses
    - `Functor`
    - `Foldable`
    - `Traversable`
    - `State`
    - `Monad`
  - Waar kun je documentatie vinden?
  = II. De opdracht voor vandaag
  = III. Mogelijke oplossingen
]

#page(fill: thema.lighten(20%), align(bottom + left, text(white, 48pt, [*I.* Typeclasses])))

#page[
  = Wat zijn typeclasses?
  - Stel dat je verschillende types hebt (Int, Point, List, Tree...)
  - Je wilt een generieke `show :: a -> String` die elk van deze types omzet naar een printbare string
  - Of je wilt een generieke `(==) :: a -> a -> Bool` die voor elk type werkt
]

#page[
  = Wat zijn typeclasses?
  #definition[
    Een typeclass is een set van functies met regels die een type kan implementeren
  ]
  - Gelijkaardige concept:
    - Interfaces in OGP
    #grid(
      columns: (60%, 40%),
      rows: (auto),
      gutter: (3pt),
      align(horizon)[ 
        ```java
        public interface Equals {
            public boolean equals(Object other);
        }
        ```
      ],
      align(horizon)[
        ```haskell
        class Equals t where
            (==) :: t -> t -> Bool
            (/=) :: t -> t -> Bool
        ```
      ]
    )
]

#page[
  = Hoe pas je een typeclass toe?
    #grid(
      columns: (50%, 40%),
      rows: (auto),
      gutter: (3pt),
      align(horizon)[
        ```haskell
        data Color = Red | Green | Blue
        ```
      ],
      align(horizon)[
        ```haskell
        class Equals t where
            (==) :: t -> t -> Bool
            (/=) :: t -> t -> Bool
        ```
      ]
    )

    #v(30pt)
    
    #grid(
      columns: (40%, 10%, 40%),
      rows: (auto),
      gutter: (3pt),
      align(horizon)[
        ```haskell
        instance Eq Color where
            Red   == Red   = True
            Green == Green = True
            Blue  == Blue  = True
            _     == _     = False
        ```
      ],
      align(horizon)[OF],
      align(horizon)[ 
        ```haskell
        instance Eq Color where
            Red   /= Red   = False
            Green /= Blue  = False
            Blue  /= Blue  = False
            _     /= _     = True
        ```
      ]
    )
]

#page[
  = Nuttige typeclasses: `Functor` (`<$>`)
  #definition[
    Een type `f` is een `Functor` indien het een functie `fmap` voorziet die, gegeven eenderwelke types a en b, je een functie van type `(a -> b)` laat toepassen om `f a` naar `f b` te veranderen, waarbij de structuur van f behouden blijft.
    
    #align(center)[
      ```haskell
      class Functor f where
          fmap :: (a -> b) -> f a -> f b
      ```
    ]
  ]
  #align(center)[
    ```haskell
    data MyList = Empty | Cons a (MyList a)
    
    instance Functor MyList where
        fmap _ Empty = Empty
        fmap f (Cons head tail) = Cons (f head) (fmap f tail)
    ```
  ]
  #align(center)[
    ```haskell
    >> fmap length (Cons "Hello " (Cons "World" (Cons "!" Empty)))
    (Cons 6 (Cons 5) (Cons 1 Empty))
    ```
  ]
]

#page[
  = Nuttige typeclasses: `Foldable`
  #definition[
    Een type `t` is `Foldable` indien het een manier voorziet om alle elementen in een structuur van links naar rechts te “samenvouwen” tot één waarde.
    Dit gebeurt met een beginwaarde en een functie die het volgende elementmet de huidige accumulator combineert.
    
    #align(center)[
      ```haskell
      class Foldable t where
         foldMap :: Monoid m => (a -> m) -> t a -> m
         foldr   :: (a -> b -> b) -> b -> t a -> b
      ```
    ]
  ]
]

#page[
  = Nuttige typeclasses: `Foldable`
  #align(center)[
    ```haskell
    data MyList = Empty | a (MyList a)
    
    instance Foldable MyList where
        foldr _ z Empty = z
        foldr f z (Cons head tail) = f head (foldr f z tail)
    // OF
    instance Foldable MyList where
        foldMap _ Empty = mempty
        foldMap f (Cons head tail) = f head <> foldMap f tail
    ```
    
  ]
  #align(center)[
    ```haskell
    >> foldr (++) "" (Cons "Hello " (Cons "World" (Cons "!" Empty)))
    "Hello World!"
    >> foldMap id (Cons "Hello " (Cons "World" (Cons "!" Empty)))
    "Hello World!"
    ```
  ]
]

#page[
  = Nuttige typeclasses: `Foldable`
  Set dat het type `t` behoort tot de `Foldable` typeclass, dan kan je ook de volgende functies gebruiken:
  - `fold    :: Monoid m => t m -> m`
  - `foldl   :: (b -> a -> b) -> b -> t a -> b`
  - `toList  :: t a -> [a]`
  - `null    :: t a -> Bool`
  - `length  :: t a -> Int`
  - `elem    :: Eq a => a -> t a -> Bool`
  - `maximum :: Ord a => t a -> a`
  - `minimum :: Ord a => t a -> a`
  - `sum     :: Num a => t a -> a`
  - `product :: Num a => t a -> a`
]

#page[
  = Nuttige typeclasses: `Traversable` (`<*>`)
  #definition[
    Een type `t` is `Traversable` indien het elementen één voor één
    kan “doorlopen” met een effectvolle functie, en zo een
    gestructureerd effect `f (t b)` oplevert.
    
    #align(center)[
      ```haskell
      class (Functor t, Foldable t) => Traversable t where
          traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
          sequenceA :: Applicative f => t (f a) -> f (t a)
      ```
    ]
  ]
  #align(center)[
    ```haskell
    data MyList = Empty | Cons a (MyList a)
    
    instance Traversable MyList where
        traverse _ Empty = pure Empty
        traverse toApp (Cons head tail) = Cons <$> toApp head <*> traverse toApp tail
    ```
  ]
]

#page[
  = Nuttige typeclasses: `Traversable` voorbeelden
  ```haskell
  safeNonEmpty :: String -> Maybe String
  safeNonEmpty "" = Nothing
  safeNonEmpty s  = Just s

  >> traverse safeNonEmpty (Cons "Hello " (Cons "World" (Cons "!" Empty)))
  Just (Cons "Hello " (Cons "World" (Cons "!" Empty)))
  
  >> traverse safeNonEmpty (Cons "Hello " (Cons "" (Cons "!" Empty)))
  Nothing
  ```

  - Alle strings niet leeg `=>` `Just (Cons ...)`
  - Minstens 1 lege string `=>` `Nothing`
]

#page[
  = Nuttige typeclasses: `Traversable` voorbeelden
  ```haskell
  dup :: String -> [String]
  dup s = [s, s ++ s]
  
  >> traverse dup (Cons "Hello" (Cons "World" Empty))
  [Cons "Hello" (Cons "World" Empty),
   Cons "Hello" (Cons "WorldWorld" Empty),
   Cons "HelloHello" (Cons "World" Empty),
   Cons "HelloHello" (Cons "WorldWorld" Empty)]
  ```

  `traverse` + lijst `->` Cartesiaans product van mogelijkheden
]

#page[
  = Nuttige typeclasses: `Traversable` voorbeelden
  ```haskell
  askQuestion :: String -> IO String
  askQuestion q = putStrLn q >> getLine
  
  >> traverse askQuestion ["What are your names?", "Favourite language?"]
  ``````text
  What are your names?
  Simeon and Jonas
  Favourite language?
  Haskell
  ``````haskell
  ["Simeon and Jonas","Haskell"]
  ```
]

#page[
  = Nuttige typeclasses: `Monad`
  TODO
]



#page(fill: thema.lighten(20%), align(bottom + left, text(white, 48pt, [*II.* De opdracht voor vandaag])))


#page[
  = TODO
]

#page(fill: thema.lighten(20%), align(bottom + left, text(white, 48pt, [*Mogelijke oplossingen*])))


#page[
  = TODO
]
