--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.ParamInfo
-- Description : Representing ParamInfo requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing ParamInfo requests.
--
--------------------------------------------------------------------
module MediaWiki.API.Action.ParamInfo where

data ParamInfoRequest
 = ParamInfoRequest
    { paModules      :: [String]
    , paQueryModules :: [String]
    }

emptyParamInfoRequest :: ParamInfoRequest
emptyParamInfoRequest = ParamInfoRequest
    { paModules      = []
    , paQueryModules = []
    }

data ParamInfoResponse
 = ParamInfoResponse
    { parModules :: [APIModule]
    }

data APIModule
 = APIModule
    { modName        :: String
    , modClass       :: String
    , modDescription :: String
    , modParams      :: [ModuleParam]
    }

data ModuleParam
 = ModuleParam
    { modParamName        :: String
    , modParamDefault     :: String
    , modParamDescription :: String
    , modParamPrefix      :: String
    , modParamType        :: ParamType
    }

data ParamType
 = TypeBool
 | TypeString
 | TypeInteger
 | TypeTimestamp
 | TypeName String
 | TypeEnum [String]
