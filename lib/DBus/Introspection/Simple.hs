{-# LANGUAGE OverloadedStrings #-}

-- Copyright notes...

-- | Simple D-Bus introspection method.
--
-- Example: export introspection of object.
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- //
-- import DBus
-- import DBus.Client
-- import DBus.Introspection.Simple
-- //
-- import Control.Concurrent
-- import Control.Monad
-- //
-- main :: IO ()
-- main = do
--   con \<- 'connectSession'
--   //
--   \-- Request a bus name for service
--   rep \<- 'requestName' con \"org.example\" ['nameReplaceExisting']
--   case rep of
--    'NamePrimaryOwner' -> do
--      'export' con \"/\" [
--        \-- Add introspection method
--        'introspectionMethod'
--        (\"/\", -- Path
--         \-- Interfaces
--         [ (\"org.example\",
--            \-- Methods
--            [ (\"Say\",
--               \-- In args
--               [ (\"text\", TypeString) \-- str in arg
--               , (\"deep\", TypeInt32) \-- int in arg
--               ],
--               \-- Out args
--               [ (\"echo\", TypeString) \-- str out arg
--               ]
--              )
--            ],
--            \-- Signals
--            [ (\"Tick\",
--               \-- Args
--               [ (\"count\", TypeInt32) \-- int arg
--               ]
--              )
--            ],
--            \-- Properties
--            [ (\"Rate\", TypeInt32, True, True) \-- int rw prop
--            ]
--           )
--         ],
--         \-- Sub paths
--         [ \"\/more\" \-- some object
--         ]
--        )
--        ]
--      export con \"\/more\" []
--      forever $ threadDelay 50000
--    _ -> error \"Cannot request name\"
-- @
--

module DBus.Introspection.Simple
       (
         -- * Definition types
         ObjectDef
       , InterfaceDef
       , MethodDef
       , SignalDef
       , PropertyDef
       , ArgumentDef
         -- * Introspect method
       , introspectionMethod
       ) where

import Data.Maybe (fromJust)
import DBus hiding (signal)
import DBus.Client (Method, autoMethod)
import DBus.Introspection hiding (Method)

-- | Object definition.
--
-- @
-- (\"/\",                 -- object path
--  [ {- ... -} ],       -- object interfaces
--  [\"\/some\", \"\/other\"]) -- sub object paths
-- @
--
type ObjectDef = (ObjectPath, [InterfaceDef], [ObjectPath])

-- | Interface definition.
--
-- @
-- (\"org.example.SomeInterface\", -- interface name
--  [ {- ... -} ],               -- interface methods
--  [ {- ... -} ],               -- interface signals
--  [ {- ... -} ])               -- interface properties
-- @
--
type InterfaceDef = (InterfaceName, [MethodDef], [SignalDef], [PropertyDef])

-- | Method definition.
--
-- @
-- (\"SomeMethod\",  -- method name
--  [ {- ... -} ], -- input arguments
--  [ {- ... -} ]) -- output arguments
-- @
--
type MethodDef = (MemberName, [ArgumentDef], [ArgumentDef])

-- | Signal definition.
--
-- @
-- (\"SomeSignal\",  -- signal name
--  [ {- ... -} ]) -- arguments
-- @
--
type SignalDef = (MemberName, [ArgumentDef])

-- | Property definition.
--
-- @
-- (\"SomeProperty\", -- property name
--  TypeInt32,      -- type
--  True,           -- readable
--  False)          -- writable
-- @
--
type PropertyDef = (String, Type, Bool, Bool)

-- | Argument definition.
--
-- @
-- (\"SomeArgument\", -- argument name
--  TypeInt32)      -- type
-- @
--
type ArgumentDef = (String, Type)

-- | Define an introspect method with the given object definition.
--
introspectionMethod :: ObjectDef -> Method
introspectionMethod = autoMethod "org.freedesktop.DBus.Introspectable" "Introspect" . introspection

introspection :: ObjectDef -> IO String
introspection = return . fromJust . formatXML . formatObject
  where
    formatMethodArg dir (name, typ) =
      methodArg name typ dir
    
    formatMethod (name, args, rets) =
      (method name) { methodArgs = map (formatMethodArg directionIn) args ++
                                   map (formatMethodArg directionOut) rets
                    }
    
    formatSignalArg (name, typ) = signalArg name typ
    
    formatSignal (name, args) =
      (signal name) { signalArgs = map formatSignalArg args
                    }

    formatProperty (name, typ, readable, writeable) =
      (property name typ) { propertyRead = readable
                          , propertyWrite = writeable
                          }
    
    formatInterface (name, methods, signals, properties) =
      (interface name) { interfaceMethods = map formatMethod methods
                       , interfaceSignals = map formatSignal signals
                       , interfaceProperties = map formatProperty properties
                       }

    dbusIntrospectableInterface =
      ("org.freedesktop.DBus.Introspectable",
       [ ("Introspect",
          [],
          [ ("xml", TypeString) ])
       ],
       [],
       [])

    formatPath path = object path
    
    formatObject (path, interfaces, children) =
      (object path) { objectInterfaces = map formatInterface
                                         (dbusIntrospectableInterface : interfaces)
                    , objectChildren = map formatPath children
                    }
