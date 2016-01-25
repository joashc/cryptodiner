{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}

module DcNodeOperator where

import Messaging
import Control.Monad.Free
import Control.Monad.Free.TH

data DcNodeOperator state incoming outgoing recipient error next =
    InitState next
  | GetIncoming (incoming -> next)
  | AwaitStateCondition (state -> Bool) (state -> next)
  | SendOutgoing recipient outgoing next
  | GetState (state -> next)
  | GetUserInput (String -> next)
  | ModifyState (state -> state) next
  | DisplayMessage String next
  | GetRandomInt Int (Int -> next)
  | Throw error next
  deriving (Functor)

-- Automagic the free monadic actions
makeFree ''DcNodeOperator
