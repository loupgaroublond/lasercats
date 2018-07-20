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
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module DevelMain where

import Control.Lens
import Control.Monad
import Control.Lens
import Data.String
import Data.Text
import Network.AWS
import Network.AWS.EC2
import Network.AWS.EC2.RunInstances
import Network.AWS.EC2.StopInstances
import Network.AWS.Data.Text
import System.IO

import LaserCats

update = putStrLn "update!"
-- s1 = Command $ Vec [LaunchInstance $ IType "t2.small"]
-- s2 = Command $ Sca $ ChangeInstanceType $ IType "t2.large"

instanceOf ::
  (Data.String.IsString a, Eq a) => a -> Maybe InstanceType
instanceOf "t2.small" = Just T2_Small
instanceOf "t2.large" = Just T2_Large
instanceOf _ = Nothing

data LaunchInstance = LaunchInstance
  { _liInstanceSize :: Text
  }
makeLenses ''LaunchInstance

instance MonadAWS m => Interpreter LaunchInstance m where
  type Instruction LaunchInstance m = m Reservation
  interpret c = let
    imageId = pure "ami-7f43f307"
    instanceSize = c ^. liInstanceSize . to instanceOf
    in
    send $ runInstances 1 1 & risInstanceType .~ instanceSize & risImageId .~ imageId

data TerminateInstance = TerminateInstance
  { _tiInstanceId :: Text
  }
makeLenses ''TerminateInstance

instance MonadAWS m => Interpreter TerminateInstance m where
  type Instruction TerminateInstance m = m TerminateInstancesResponse
  interpret c = let
    instanceId = pure $ c ^. tiInstanceId
    in
    send $ terminateInstances & tiInstanceIds .~ instanceId

data ChangeInstanceType = ChangeInstanceType
  { _citInstanceId :: Text
  , _citInstanceSize :: Text
  }
makeLenses ''ChangeInstanceType

instance MonadAWS m => Interpreter ChangeInstanceType m where
  type Instruction ChangeInstanceType m = m (StopInstancesResponse, ModifyInstanceAttributeResponse, StartInstancesResponse)
  interpret c = let
    instanceValue = c ^. citInstanceId . to pure
    instanceSize = c ^. citInstanceSize . to pure
    instanceFilter = (filter' "instance-id") & fValues .~ instanceValue
    diCom = describeInstances & diiFilters .~ pure instanceFilter
    sizeAttribute = pure $ attributeValue & avValue .~ instanceSize
    in do
    stopInstancesResponse <- send (stopInstances & siInstanceIds .~ instanceValue)
    instanceStopped `await` diCom
    modifyInstanceAttributeResponse <- send $ (modifyInstanceAttribute $ c ^. citInstanceId) & mInstanceType .~ sizeAttribute
    startInstancesResponse <- send (startInstances & sInstanceIds .~ instanceValue)
    instanceRunning `await` diCom
    -- fix these to use describeinstancestatus
    -- systemStatusOK `await` diCom
    -- instanceStatusOK `await` diCom
    return (stopInstancesResponse, modifyInstanceAttributeResponse, startInstancesResponse)

s1 = LaunchInstance "t2.small"
s2 id = ChangeInstanceType id "t2.large"
s3 id = TerminateInstance id

logr :: IO Logger
logr = newLogger Debug stdout
envi :: IO Env
envi = newEnv $ FromFile "LaserCats" "/Users/yankee/.aws/credentials"
awsAct :: Logger -> Env -> AWS a -> IO a
awsAct lgr env act = runResourceT $ runAWS (env & envLogger .~ lgr ) $ within Oregon $ act
action act = join $ awsAct <$> logr <*> envi <*> pure act
