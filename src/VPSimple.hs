{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
--
-- Author: Ivan
-- File: VPSimple.hs
-- Data: 2013-04-07


module VPSimple where

import Data.List
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Applicative
import Debug.Trace
import Data.Map(Map)

import Data.Tree
import System.FilePath
import Control.Exception
import Prelude hiding (catch)
import System.IO.Error hiding(catch)
import System.FilePath.Windows


{- {{{
data Line =
    CodeLine String |
    BeginFile String |
    EndFile String
    deriving(Eq,Ord,Show,Read)

data File a = File {
    file_context :: [FilePath],
    file_code :: String }

show_line (CodeLine s) = s
show_line (BeginFile f) = "//{" ++ f
show_line (EndFile f) = "//}" ++ f

-- type Code = [Line]

data TokenType = 
    Ifdef String |
    Ifndef String |
    Define String |
    Include String |
    Endif |
    Code Line
    deriving(Eq,Ord,Show,Read)

data Token = Token Int FilePath TokenType
    deriving(Eq,Ord,Show,Read)

mk_token :: String -> TokenType
mk_token s = case mapM go ts of
        Left x -> x
        Right _ -> error $ "Can't find token for string `" ++ s ++ "`"
    where
    ts = [(stripPrefix "`ifndef ",Ifndef)
         ,(stripPrefix "`ifdef ",Ifdef )
         ,(stripPrefix "`endif",const Endif)
         ,(stripPrefix "`define",Define)
         ,(stripPrefix "`include",Include)
         ,(Just,Code . CodeLine)]

    go (f,f') = case f s of
        Nothing -> Right "Can't find token"
        Just x  -> Left $ f' x
        
tokenize :: FilePath -> [String] -> [Token]
tokenize fname ts =  zipWith (\i t ->  Token i fname $ mk_token t ) [0..] ts

parseFile fname = pure ((tokenize fname) . lines )  <*> readFile fname

include_file :: [Token] -> String -> IO [Token]
include_file ts = (liftM (++ts) ) . parseFile . read

-- parseTree = unfoldTreeM 
-}

{-
data ParserState = ParserState
    {   ptokens :: [Token]
    ,   pdefines :: [String]
    ,   pcode :: Map Context [Line]
    ,   pifs :: [Token]
    ,   preadFile :: (FilePath -> IO String)
    ,   pio :: IO ()
    ,   pcontext :: Context
    }

data Line
data Context


type PState = State ParserState

modifyIfdef :: ([Token] -> [Token]) -> PState ()
modifyIfdef f = modify $ \p@(pifs -> ifs) -> p { pifs = f ifs }

eval_token :: PState ()
eval_token = do
    nt <- hasNext
    case nt of
        Nothing -> return ()
        Just t -> eval t
    where
    eval (token_type -> Ifdef s) = do
        modifyIfdef (t:)
        defs <- gets pdefines
        if s `elem` defs 
            then eval_token
            else wait_endif

    eval t@(token_type -> Ifndef s) = do
        modifyIfdef (t:)
        defs <- gets pdefines
        if s `elem` defs
            then wait_endif
            else eval_token

    eval t@(token_type -> Include s) = do   
        modifyIfdef
    
    wait_endif = do
        nt <- hasNext
        case nt of
            Nothing -> error "Error: EOF, Expected Endif"
            Just (token_type -> Endif) -> modifyIfdef (tail) >> eval_token
            _ -> wait_endif


    hasNext = undefined

-}

{-
type Result = Either (ParserResult String)

data ParserResult a = 
    ParserError a |
    ParserDone (Map Context [Line])
    deriving(Eq,Ord,Show,Read,Functor)

tokenize :: FilePath -> [String] -> [Token]
tokenize fname ts =  zipWith (\i t ->  Token i fname $ mk_token t ) [0..] ts
    where
    mk_token :: String -> TokenType
    mk_token s = case mapM go ts of
            Left x -> x
            Right _ -> error $ "Can't find token for string `" ++ s ++ "`"
        where
        ts = [(stripPrefix "`ifndef ",Ifndef)
             ,(stripPrefix "`ifdef ",Ifdef )
             ,(stripPrefix "`endif",const Endif)
             ,(stripPrefix "`define",Define)
             ,(stripPrefix "`include",Include)
             ,(Just,Code )]

        go (f,f') = case f s of
            Nothing -> Right "Can't find token"
            Just x  -> Left $ f' x

data Line = Line String
    deriving(Eq,Ord,Show,Read)

type Context = [FilePath]
data ParserInfo = ParserInfo {
    pi_tokens :: [Token],
    pi_defines :: [String],
    pi_code :: Map Context [Line],
    pi_ifs :: [Token],
    pi_readFile :: (FilePath -> IO String),
    pi_context :: Context
    }

type PState = StateT (Result ParserInfo) IO

addTokens :: [Token] -> PState ()
addTokens ts = modify $ fmap (\p -> p { pi_tokens = ts ++ (pi_tokens p) })

include_file :: String -> PState ()
include_file s = do
    fcont <- liftIO $ readFile s
    let extToks = tokenize s $ lines fcont
    addTokens extToks
    eval_tokens
    return ()

pAddDefine s p@(pi_defines -> defs) = p { pi_defines = s : defs }

getTokens :: PState (Result [Token])
getTokens = gets $ fmap (pi_tokens)

getTokenTypes :: PState (Result [TokenType])
getTokenTypes = gets $ fmap ( (map token_type) . pi_tokens)

hasNextToken :: PState (Maybe Token)
hasNextToken = do
    p <- get
    case p of
        Left (ParserDone _) -> return Nothing
        Left (ParserError s) -> return Nothing
        Right p -> case (pi_tokens p) of
            [] -> do
                put $ Left (ParserDone (getCode p))
                return Nothing
            (t:ts) -> do
                modify $ fmap (\p -> p { pi_tokens = ts })
                return $ Just t

getCode (pi_code -> code) = code

eval_tokens :: PState ()
eval_tokens = do
    nt <- hasNextToken
    tryInclude nt include_file
    return ()
    where
    tryInclude (Just (token_type -> Include s)) m = m s
    tryInclude _ m = return ()
-}

{-
    go (Left x) = Left x
    go (Right p@(pi_tokens -> [])) = Left ParserDone
    go (Right p@(pi_tokens -> ( (token_type -> t):ts))) = 
            case t of
                Define s -> Right $ pAddDefine s p
                Ifdef s | s `elem` defs -> Right $ p
                        | otherwise -> waitEndif p
                Endif | defs == [] -> Left $ ParserError "Unexpected Endif"
                      | otherwise -> Right $ p
        where
        defs = pi_defines p
    
    waitEndif = return

runParser = fmap snd $ runStateT (include_file "") init_state

init_state = Right $ ParserInfo 
    {   pi_tokens = []
    ,   pi_defines = []
    ,   pi_code = Map.empty
    ,   pi_ifs = []
    ,   pi_readFile = readFile
    ,   pi_context = []
    }

-}

{-
data Parser a = Parser [(Token a => a -> PState -> IO (PState))]

data PState

class Token a where
    mkToken :: String -> Maybe a


data Ifdef = Ifdef String
data Endif
data Define = Define String
data Include = Include String

instance Token Ifdef where
    mkToken (stripPrefix "`ifdef" -> a) = fmap Ifdef  a

instance Token Endif where
    mkToken (stripPrefix "`endif" -> a) = fmap (undefined :: a -> Endif)  a

instance Token Define where
    mkToken (stripPrefix "`define" -> a) = fmap Define  a

instance Token Include where
    mkToken (stripPrefix "`include " -> a) = fmap Include  a
-}

{-
    Ifdef String |
    Ifndef String |
    Define String |
    Include String |
    Endif |
    Code Line
}}} -}

data ParserInfo = ParserInfo {
    pi_tokens :: [Token],
    pi_defines :: [String],
    pi_code :: [Line],
    pi_ifs :: [Token],
    pi_readFile :: FileRead,
    pi_context :: [String]
    }
    deriving(Eq,Ord,Show)

newtype FileRead = FileRead (FilePath -> IO String)

instance Eq FileRead where
    a == b = False
    
instance Show FileRead where
    show _ = "readFile"

instance Ord FileRead where
    a < b = False

data Line =
    CodeLine String |
    BeginFile String |
    EndFile String |
    SourceLine String [FilePath] Int
    deriving(Eq,Ord,Show,Read)

show_line (CodeLine s) = s
show_line (BeginFile f) = "//{" ++ f
show_line (EndFile f) = "//}" ++ f
show_line (SourceLine code context line) = code ++ ("//" ++ (show $ FileLine context line))

-- show_line (SourceLine code context line) = code ++ "//" ++ file ++ ":" ++ (show line)
--    where file = head context

data FileLine = FileLine [String] Int
    deriving(Eq,Ord,Show,Read)

-- show_line (SourceLine code context line) = code ++ ("//{" ++ path ++ "}#" ++ (show line))
--    where path = concat $ intersperse "#" context


data TokenType = 
    Ifdef String |
    Ifndef String |
    Define { tt_define :: String } |
    Include String |
    Endif |
    Comment |
    Code { tt_code :: Line }
    deriving(Eq,Ord,Show,Read)

data Token = Token Int FilePath TokenType
    deriving(Eq,Ord,Show,Read)

mk_token :: String -> TokenType
mk_token s = case mapM go ts of
        Left x -> x
        Right _ -> error $ "Can't find token for string `" ++ s ++ "`"
    where
    ts = [(stripPrefix "`ifndef ",Ifndef)
         ,(stripPrefix "`ifdef ",Ifdef )
         ,(stripPrefix "`endif",const Endif)
         ,(stripPrefix "`define",Define)
         ,(stripPrefix "`include",Include)
         ,(Just,Code . CodeLine)]

    stripSpace s = reverse $ dropWhile (==' ') $ reverse $ dropWhile (==' ') s

    stripComment :: String -> String
    stripComment str = 
        let (code,comment) = findComment str
         in code
    

    go (f,f') = case f s of
        Nothing -> Right "Can't find token"
        Just x  -> Left $ f' $ stripSpace $ stripComment x
        
tokenize :: FilePath -> [String] -> [Token]
tokenize fname ts =  zipWith (\i t ->  Token i fname $ mk_token t ) [0..] ts

fieldTokens  f p@ParserInfo {..} = p { pi_tokens = f pi_tokens}
fieldIfs     f p@ParserInfo {..} = p { pi_ifs = f pi_ifs }
fieldDefines f p@ParserInfo {..} = p { pi_defines = f pi_defines }
fieldCode    f p@ParserInfo {..} = p { pi_code = f pi_code }
fieldContext f p@ParserInfo {..} = p { pi_context = f pi_context }


eval_token :: Bool -> ParserInfo -> IO ParserInfo

eval_token _ p@(pi_tokens -> []) = return p

eval_token noskip p@(ParserInfo ( t@(Token lNum fName tt)  :ts) defs code ifs _ context) = 
        if noskip then
            case tt of
                Include s -> include_file next_p s
                Ifdef s -> nextIf  (s `elem` defs) withIf
                Ifndef s -> nextIf (not (s `elem` defs)) withIf
                -- Ifndef s -> nextIf (not (s `elem` defs)) withIf
                Endif | ifs == [] -> error $ "In file " ++ (show fName) ++ "at line: " ++ (show lNum)  ++ " Error: Ifdef/Ifndef missing" ++ (show p)
                      | otherwise -> next withCloseIf
                Code (CodeLine s) -> next (withSourceLine s)
                Code (BeginFile s) -> next (withEnterContext s)
                Code (EndFile s) -> next (withLeaveContext s)
                Define _ -> next withDef
                Comment -> next id
            else
                case tt of
                    Endif -> next withCloseIf
                    _ -> nextIf False id
    where
    nextIf b f = eval_token b (f next_p)
    next f = nextIf True f
    
    withEnterContext s = (fieldContext (s:))
    withLeaveContext s = (fieldContext (tail))

    withSourceLine s = fieldCode ((SourceLine s context lNum):)

    withIf       = fieldIfs (t:) -- next_p                 -- { pi_ifs      = t : ifs }
    withCodeLine = fieldCode (tt_code tt :) -- next_p      -- { pi_code     = (tt_code tt)   : code }
    withDef     = fieldDefines (tt_define tt :) -- next_p -- { pi_defines  = (tt_define tt) : defs }
    withCloseIf = fieldIfs tail -- next_p                 -- { pi_ifs      = tail ifs   }

    next_p = p { pi_tokens = ts }

include_file p filename = do
    tokens <- parseFile reader (read filename)
    eval_token True $ new_p (begin_file : (tokens ++ [end_file]))
    where
    begin_file = Token (-1) filename $ Code $ BeginFile filename
    end_file = Token (-1) filename $ Code $ EndFile filename
    reader = pi_readFile p

    new_p newts = let ts = pi_tokens p
             in p { pi_tokens = newts ++ ts}

parseFile (FileRead fread) fname = pure ((tokenize fname) . lines )  <*> fread fname

findComment str =(\(x,y) -> ((concat x),(concat y)) ) $ span (not . isComment)  (groupBy gf str)
    where
    isComment = (==) "//"
    gf '/' '/' = True
    gf _   _   = False

translate :: Int -> Int -> IO ()
translate line column = do
    fcont <- getContents
    let markers = map mk_marker $ lines fcont
    let (FileLine context real_line)=markers!!(line + 1)
    putStrLn $ ("In include stack " ++ (show_stack $ reverse context)) ++ (":" ++ (show real_line) ++ ":" ++ (show column))
    return ()
    where

    show_stack sc = intercalate ",\n\t" sc

    mk_marker :: String -> FileLine
    mk_marker str = read $ concat $ drop 1 $ dropWhile (\s -> s /= "//" ) $ groupBy gf str
        where
        gf '/' '/' = True
        gf _   _   = False


fileReader basefile filename = do
    fcont <- (\c -> catch readWithBaseDir c) $ \e ->
        if not $ isDoesNotExistError e then error $ show e
        else readWithCurrentDir
    return fcont
    where
    readWithCurrentDir = readFile filename
    
    readWithBaseDir :: IO String
    readWithBaseDir = do
        let fname = (fst $ splitFileName basefile) </> (snd $ splitFileName filename)
        readFile fname
    
    
        

parse :: FilePath -> IO String
parse fname = do
--    ts <- parseFile fname
    let defs = []
    let p = ParserInfo [Token 0 fname (Include (show fname) ) ] defs [] [] (FileRead $ fileReader $ fname) [] :: ParserInfo
    newp@(ParserInfo _ _ code _ _ _) <- eval_token True p 
    return $ unlines $ map show_line $ reverse code
--    return $ show newp


