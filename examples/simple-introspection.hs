{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import DBus
import DBus.Client
import DBus.Introspection.Simple

import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
  con <- connectSession
  
  -- Request a bus name for service
  rep <- requestName con "org.example" [nameReplaceExisting]
  case rep of
   NamePrimaryOwner -> do
     export con "/" [
       -- Add introspection method
       introspectionMethod
       ("/", -- Path
        -- Interfaces
        [ ("org.example",
           -- Methods
           [ ("Say",
              -- In args
              [ ("text", TypeString) -- str in arg
              , ("deep", TypeInt32) -- int in arg
              ],
              -- Out args
              [ ("echo", TypeString) -- str out arg
              ]
             )
           ],
           -- Signals
           [ ("Tick",
              -- Args
              [ ("count", TypeInt32) -- int arg
              ]
             )
           ],
           -- Properties
           [ ("Rate", TypeInt32, True, True) -- int rw prop
           ]
          )
        ],
        -- Sub paths
        [ "/more" -- some object
        ]
       )
       ]
     export con "/more" []
     forever $ threadDelay 50000
   _ -> error "Cannot request name"
