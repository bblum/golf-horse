import Data.Bits
import Data.List
import Data.List.Extra
import Data.Maybe
import System.Environment

----------------------------
-- indexing/prefix-coding --
----------------------------

-- @     = newline & update "chapter number" (a->b->c->...) & clears the prefix
-- 1x    = newline & specify 1-length prefix (initial letter + x + following suffix)
-- 2xy   = newline & specify 2-length prefix (initial letter + xy + following suffix)
-- 3xyz  = newline & specify 3-length prefix (initial letter + xyz + following suffix)
-- 4xyzw = newline & specify 4-length prefix (initial letter + xyzw + following suffix)
-- !     = newline within prefixified group
-- there's no more bc those 6 charas + the 26 alpha = 32, for a 5 bit pack

-- example: wordlist "bar,foobar,foobaz,quux,quuux" could be encoded as
-- "@ar@@@@4oobar!z@@@@@@@@@@@2uux!ux"
-- words that don't share prefixes with any other words must go at the beginning
-- of their chapter, before any prefixed words, bc @ does double duty between
-- advancing the chapter "number" and clearing the prefix

newLine = "!"
newChap = "@"
maxPrefix = 4
packWidth = 5
prefixFactor = 2 -- allows e.g. 4 to really mean 8/12/etc; useful for wordlist.asc

-- nb. 0s have to go in front of all actually prefixified stuff!
siftPrefixes :: Int -> (String, [String]) -> (String, [String])
siftPrefixes 0 (output,input) = (intercalate newLine input ++ output, [])
siftPrefixes n (output,input) = (concatMap prefixify ps ++ output, concat rest) where
    truePrefixLength = prefixFactor * n
    (ps,rest) = partition ((>1) . length) $ groupBy sharePrefix input
    sharePrefix x y | length (zip x y) < truePrefixLength = False
    sharePrefix x y = and $ take truePrefixLength $ zipWith (==) x y
    -- turns e.g. ["foobar","foobaz"] into "5foobar!z", supposing n==5
    prefixify ps = show n ++ take truePrefixLength (head ps) ++
                   intercalate newLine (map (drop truePrefixLength) ps)

-- nb. have to count down (i.e. foldr) to not just oops 1-length prefixify everything
addPrefixesToOneChapter :: [String] -> String
addPrefixesToOneChapter grupo = fst $ foldr siftPrefixes ([],grupo) [0..maxPrefix]

indexify :: [[String]] -> String
indexify grupos = intercalate newChap $ map (addPrefixesToOneChapter . map tail) grupos

--------------------------
-- sub-byte compression --
--------------------------

toBits 0 = []
toBits n = (mod n 2):(toBits $ div n 2) -- little endian

fromBits [] = 0
fromBits (b:bs) = b .|. (shiftL (fromBits bs) 1) -- same tbh

pad bits | length bits > packWidth = error "bits too long already"
pad bits = take packWidth $ bits ++ repeat 0

encode :: Char -> [Int]
encode c | c == head newChap = pad $ toBits 0
encode c | c == head newLine = pad $ toBits 1
encode c | c == '0' = error "please...."
encode c | c <= head (show maxPrefix) = pad $ toBits $ 1 + fromEnum c - fromEnum '0'
encode c = pad $ toBits $ 2 + maxPrefix + fromEnum c - fromEnum 'a'

escape '`'  = ("\\`", 1)
escape '$'  = ("\\$", 1)
escape '\r' = ("\\r", 1)
escape '\\' = ("\\\\", 1)
escape c    = ([c], 0)

compress :: String -> (String, Int) -- returns buffer + num escaped charas therein
compress input = (concat output, sum numEscaped) where
    byteBuffer = chunksOf 8 $ concatMap encode input
    (output, numEscaped) = unzip $ map (escape . toEnum . fromBits) byteBuffer

------------------------
-- decompression (js) --
------------------------

-- golfs a few charas off the initialization of 'n' when factor is 1 (ten-hundred)
golfPrefixLength = if prefixFactor == 1 then "x-1" else show prefixFactor ++ "*x-" ++ show prefixFactor

jsify chapters (buffer, numEscaped) =
    "b=`" ++ buffer ++ "`;" ++
    "p='';" ++ -- currently active prefix, if any
    "o='a';" ++ -- output buffer (cheating by starting with 'a', bc 1-indexed chaps)
    -- c = chapter counter
    -- n = prefix length
    -- i = bit index into buffer
    -- q = "this chapter has no non-prefixified intro words" flag (e.g., 'q' chapter)
    -- i'm not sure why this -1 is necessary to avoid bss-ol-bar iteration tbh :<
    "for(q=i=n=c=0;i<" ++ show ((length buffer - numEscaped - 1) * 8) ++ ";i+=5){" ++
    "r=i%8;" ++ -- offset of 5-bit chunk into current 8-byte buffer chara
    "x=b.charCodeAt((i-r)/8)>>r&255>>r&31|(b.charCodeAt((i-r)/8+1)&15>>7-r)<<8-r;" ++
    "if(x<6){" ++ -- is one of the control charas? @ ! 1 2 3 4
    "if(!x){c++;p='';q=1}" ++ -- increment chapter; reset prefix & maybe q chapter?
    "if(x>1){p='';n=" ++ golfPrefixLength ++ ";q++}" ++ -- begin parsing prefix
    "if(q<2)" ++ -- skip following if previously @ was followed immediately by 1/2/3/4
    "o+='\\n'+'" ++ chapters ++ "'[c]+p" ++ -- start printing new word w/chap.# & prefix
    "}else{" ++ -- parse normal alpha chara
    "w=String.fromCharCode(91+x);" ++ -- 'a' (ascii 97) minus 6 control characters
    "if(n){p+=w;if(!--n)o+=p}" ++ -- stash chara into prefix
    "else{o+=w;q=0}" ++ -- no prefix active; emit it directly
    "}}console.log(o)"

main = do
    wordlistFile <- fromMaybe (error "give filename pls") <$> listToMaybe <$> getArgs
    wordlist <- lines <$> readFile wordlistFile
    let chapters = nub $ map head $ sort wordlist -- nb. "[a..w] ++ [y]" for ten-hundred
    putStrLn $ jsify chapters $ compress $ indexify $ groupOn head $ sort wordlist
