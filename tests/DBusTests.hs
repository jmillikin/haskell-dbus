-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Main
	( tests
	, main
	) where

import           Test.Chell

import           DBusTests.Address
import           DBusTests.BusName
import           DBusTests.ErrorName
import           DBusTests.Integration
import           DBusTests.InterfaceName
import           DBusTests.Introspection
import           DBusTests.MemberName
import           DBusTests.ObjectPath
import           DBusTests.Serialization
import           DBusTests.Socket
import           DBusTests.Signature
import           DBusTests.Transport
import           DBusTests.Variant

-- import all dbus modules here to ensure they show up in the coverage report,
-- even if not tested.
import           DBus ()
import           DBus.Address ()
import           DBus.Client ()
import           DBus.Client.Simple ()
import           DBus.Constants ()
import           DBus.Introspection ()
import           DBus.Message ()
import           DBus.Socket ()
import           DBus.Types ()
import           DBus.Util ()
import           DBus.Util.MonadError ()
import           DBus.Wire ()

tests :: [Suite]
tests =
	[ test_Address
	, test_BusName
	, test_ErrorName
	, test_Integration
	, test_InterfaceName
	, test_Introspection
	, test_MemberName
	, test_ObjectPath
	, test_Serialization
	, test_Signature
	, test_Socket
	, test_Transport
	, test_Variant
	]

main :: IO ()
main = Test.Chell.defaultMain tests