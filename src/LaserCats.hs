{-
   Copyright 2018 Yaakov M Nemoy

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at either

       - The LICENSE file at the root of this project
       - http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module LaserCats where

import Data.Text
import Data.String

data Doc i = Vec [i] | Sca i
  deriving (Functor, Show)

data Log i = Log [i]
  deriving (Show, Functor)

data Command c = Command (Doc c)
  deriving (Functor, Show)

class Interpreter c m where
  type Instruction c (m :: * -> *)
  interpret :: c -> Instruction c m


data Machine e d m a = Machine
  {
    runMachine :: d -> m (a, d, e)
  }

data Loader i e d m a =
  Loader {
    load :: i -> Machine e d m a
  }

type SubEvent e e' = e -> e'
type SubModel m m' = m -> m'

class Monoid e => Event e
class Model d

type Rollup d e = e -> d -> d
type Query d r = d -> r

type QueryCommand d i = d -> Command i
