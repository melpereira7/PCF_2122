\documentclass[a4paper]{article}
\usepackage[a4paper,left=2cm,right=2cm,top=2cm,bottom=2cm]{geometry}
\usepackage[english]{babel}
\usepackage{palatino}
\usepackage[colorlinks=true,linkcolor=blue,citecolor=blue]{hyperref}
\usepackage{graphicx}
\usepackage{Adventurers}
\usepackage{subcaption}
\usepackage{adjustbox}
\usepackage{color}

\definecolor{red}{RGB}{255,  0,  0}
\definecolor{blue}{RGB}{0,0,255}
\def\red{\color{red}}
\def\blue{\color{blue}}
%================= local x=====================================================%
\def\getGif#1{\includegraphics[width=0.3\textwidth]{cp2122t_media/#1.png}}
\let\uk=\emph
\def\aspas#1{``#1"}
%================= lhs2tex=====================================================%
%include polycode.fmt
%format P1 = "P_1 "
%format P2 = "P_2 "
%format P5 = "P_5 "
%format P10 = "P_{10} "
%format (div (x)(y)) = x "\div " y
%format succ = "\succ "
%format ==> = "\Longrightarrow "
%format map = "\map "
%format length = "\length "
%format fst = "\p1"
%format p1  = "\p1"
%format snd = "\p2"
%format p2  = "\p2"
%format Left = "i_1"
%format Right = "i_2"
%format i1 = "i_1"
%format i2 = "i_2"
%format >< = "\times"
%format >|<  = "\bowtie "
%format |-> = "\mapsto"
%format . = "\comp "
%format .=?=. = "\mathbin{\stackrel{\mathrm{?}}{=}}"
%format (kcomp (f)(g)) = f "\kcomp " g
%format -|- = "+"
%format conc = "\mathsf{conc}"
%format summation = "{\sum}"
%format (either (a) (b)) = "\alt{" a "}{" b "}"
%format (frac (a) (b)) = "\frac{" a "}{" b "}"
%format (uncurry f) = "\uncurry{" f "}"
%format (const (f)) = "\underline{" f "}"
%format LTree3 = "\mathsf{LTree3}"
%format (lcbr (x)(y)) = "\begin{lcbr}" x "\\" y "\end{lcbr}"
%format (split (x) (y)) = "\conj{" x "}{" y "}"
%format (for (f) (i)) = "\for{" f "}\ {" i "}"
%format B_tree = "\mathsf{B}\mbox{-}\mathsf{tree} "
\def\ana#1{\mathopen{[\!(}#1\mathclose{)\!]}}
%format <$> = "\mathbin{\mathopen{\langle}\$\mathclose{\rangle}}"
%format Either a b = a "+" b
%format fmap = "\mathsf{fmap}"
%format NA   = "\textsc{na}"
%format NB   = "\textsc{nb}"
%format inT = "\mathsf{in}"
%format outT = "\mathsf{out}"
%format outLTree = "\mathsf{out}"
%format inLTree = "\mathsf{in}"
%format inFTree = "\mathsf{in}"
%format outFTree = "\mathsf{out}"
%format Null = "1"
%format (Prod (a) (b)) = a >< b
%format fF = "\fun F "
%format k1 = "k_1 "
%format k2 = "k_2 "
%format h1 = "h_1 "
%format h2 = "h_2 "
%format f1 = "f_1 "
%format f2 = "f_2 "
%format l1 = "l_1 "
%format d1 = "d_1 "
%format d2 = "d_2 "
%format d3 = "d_3 "
%format v3 = "v_3 "
%format g1 = "g_1 "
%format g2 = "g_2 "
%format map1 = "map_1 "
%format map2 = "map_2 "
%format map3 = "map_3"
%format l2 = "l_2 "
%format Dist = "\fun{Dist}"
%format IO = "\fun{IO}"
%format LTree = "{\LTree}"
%format FTree = "{\FTree}"
%format inNat = "\mathsf{in}"
%format (cata (f)) = "\cata{" f "}"
%format (cataNat (g)) = "\cataNat{" g "}"
%format (cataList (g)) = "\cataList{" g "}"
%format (anaList (g)) = "\anaList{" g "}"
%format Nat0 = "\N_0"
%format Rational = "\Q "
%format toRational = " to_\Q "
%format fromRational = " from_\Q "
%format muB = "\mu "
%format (frac (n)(m)) = "\frac{" n "}{" m "}"
%format (fac (n)) = "{" n "!}"
%format (underbrace (t) (p)) = "\underbrace{" t "}_{" p "}"
%format matrix = "matrix"
%%format (bin (n) (k)) = "\Big(\vcenter{\xymatrix@R=1pt{" n "\\" k "}}\Big)"
%format `ominus` = "\mathbin{\ominus}"
%%format % = "\mathbin{/}"
%format <-> = "{\,\leftrightarrow\,}"
%format <|> = "{\,\updownarrow\,}"
%format `minusNat`= "\mathbin{-}"
%format ==> = "\Rightarrow"
%format .==>. = "\Rightarrow"
%format .<==>. = "\Leftrightarrow"
%format .==. = "\equiv"
%format .<=. = "\leq"
%format .&&&. = "\wedge"
%format cdots = "\cdots "
%format pi = "\pi "
%format (curry (f)) = "\overline{" f "}"
%format (cataLTree (x)) = "\llparenthesis\, " x "\,\rrparenthesis"
%format (cataFTree (x)) = "\llparenthesis\, " x "\,\rrparenthesis"
%format (anaLTree (x)) = "\mathopen{[\!(}" x "\mathclose{)\!]}"
%format delta = "\Delta "
%format eta = "\eta "
\newlabel{eq:fokkinga}{{3.93}{110}{The mutual-recursion law}{section.3.17}{}}
%format (plus (f)(g)) = "{" f "}\plus{" g "}"
%format ++ = "\mathbin{+\!\!\!+}"
%format Integer  = "\mathbb{Z}"
\def\mcond#1#2#3{#1 \rightarrow #2\;,\;#3}
%format (Cp.cond (p) (f) (g)) = "\mcond{" p "}{" f "}{" g "}"
%format (kleisli (f)) = f"^\star"
\def\plus{\mathbin{\dagger}}

%---------------------------------------------------------------------------

\title{\bfseries Modelling and Analysis of a Cyber-Physical System\\ with Monads \\ {\Large Cyber-Physical Programming --- Practical Assignment 2}}

\author{
    Melânia Pereira \quad \quad Paulo R. Pereira\\
    \texttt{\{pg47520, pg47554\}@@alunos.uminho.pt}
}

\begin{document}
\raggedbottom
\setstretch{1.25}

\maketitle

\begin{abstract}
This assignment aims to model and to analyze a system with a powerful weapon of functional programming --- monads! The system to model envolves 4 adventurers, one lantern and a bridge! This 4 adventurers need to cross the bridge, but, for safety reasons, only two people can cross at the same time and one of them needs to carry the lantern, also, each adventurer takes a different time than the others to cross the bridge. The first task of the assignment is to model this system using \textsc{Haskell} and to verify some claims made by the adventurers, namely, that they can be all on the other side in 17 minutes, and also to show that it is impossible for them all to be on the other side in less then 17 minutes. This system could be modeled using different approaches and modules, as it was done in classes via UPPAAL, so the second task of the assignment focuses on the comparison of both UPPAAL and \textsc{Haskell} approaches.
\end{abstract}

%if False
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import Cp
import DurationMonad
import ListLogDur
import ListDur
\end{code}
%endif

\section{The Adventurers' Problem}
In the middle of the night, four adventurers encounter a shabby rope-bridge spanning a deep ravine.
For safety reasons, they decide that no more than 2 people should cross the bridge at the same
time and that a flashlight needs to be carried by one of them in every crossing. They have only
one flashlight. The 4 adventurers are not equally skilled: crossing the bridge takes them 1, 2, 5,
and 10 minutes, respectively. A pair of adventurers crosses the bridge in an amount of time equal
to that of the slowest of the two adventurers.

One of the adventurers claims that they cannot be all on the other side in less than 19 minutes.
One companion disagrees and claims that it can be done in 17 minutes.

Who is right? That's what we're going to find out.

\section{Monadic Approach via \textsc{Haskell} for Modelling the Problem}
The solution is to take advantage of the non-deterministic monad (monad List) to use brute force and calculate all possible moves until we reach the final state. To deal with the time adventurers need to cross, we'll use the duration monad (already implemented by prof. Renato Neves) whose implementation adds the time each adventurer takes in a given move. This duration monad will be \aspas{encapsulated} in our final monad |ListLogDur|. This one will offer, for a certain state, a list of following states with the respective duration needed to get it and will also offer the path traveled at the moment. This path will be in a |string| and, as we'll see, it is going to be very elegant. For now, let's analyze the construction of our monad!
\subsection{The \textit{ListLogDur} monad}
As said before, we'll use our monad to have a list of states with the respective duration needed to get it and the path traveled at the moment. However, we want our monad to be parametric. So,
\begin{spec}
data ListLogDur a = LSD [Duration (String, a)] deriving Show
\end{spec}
We now have our set-constructor, so we need to define the $\eta$ function and the $(-)^\star$ operator. To define the $\eta$ function, when need to understand what means a effect-free representation in this monad --- it means that we have no duration and an empty path traveled. So,
\begin{spec}
eta : X -> ListLogDur X
eta x = LSD [Duration (0,([],x))]
\end{spec}
For the $(-)^\star$ operator, assuming a function |f : X -> ListLogDur Y|, we'll have to define the function, |kleisli f : X -> ListLogDur Y|, which in \textsc{Haskell} corresponds to the |(>>=)| operation. This implementation follows along with the |instance Functor| and the |instance Applicative| (required by \textsc{Haskell} itself).
\begin{spec}
instance Functor ListLogDur where
    fmap f = LSD . (map (fmap (id >< f))) . remLSD

instance Applicative ListLogDur where
    pure = LSD . pure . pure . (\x -> ([], x))
    l1 <*> l2 = LSD $ do
        x <- remLSD l1
        y <- remLSD l2
        g(x,y) where
            g(Duration (d1,(s,f)),Duration (d2,(s',x))) = return (Duration (d1 + d2, (s++s', f x)))

instance Monad ListLogDur where
    return = pure
    l >>= k = LSD $ do
      x <- remLSD l
      g x k where
        g (Duration (d, (s, a))) k =
          map (\(Duration (d', (s', a))) -> (Duration (d + d', (s ++ s', a)))) (remLSD (k a))

remLSD :: ListLogDur a -> [Duration (String, a)]
remLSD (LSD x) = x
\end{spec}
\subsection{Modelling the problem}
Adventurers are represented by the following data type:
\begin{code}
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)
\end{code}
Lantern is represented by the |()| element, so we can represent all the entities by using the coproduct and defining the following data type:
\begin{code}
type Object = Either Adventurer ()

lantern = Right ()
\end{code}
The names for the adventurers are quite suggestive as they are identified by the time they take to cross. However, it will be very useful to have a function that returns, for each adventurer, the time it takes to cross the bridge.
\begin{code}
getTimeAdv :: Adventurer -> Int
getTimeAdv P1 = 1
getTimeAdv P2 = 2
getTimeAdv P5 = 5
getTimeAdv P10 = 10
\end{code}
Now, we need to define the state of the game, i.e. the current position of each object (adventurers $+$ the lantern). The function |const False| represents the initial state
of the game, with all adventurers and the lantern on the left side of
the bridge. Similarly, the function |const True| represents the end
state of the game, with all adventurers and the lantern on the right
side of the bridge. We also need to define the instances |Show| and |Eq| to visualize and compare, respectively, the states of the game.
\begin{code}
type State = Object -> Bool

instance Show State where
  show s = show . show $ [s (Left P1),
                          s (Left P2),
                          s (Left P5),
                          s (Left P10),
                          s (Right ())]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]

gInit :: State
gInit = const False

gEnd :: State
gEnd = const True

state2List :: State -> [Bool]
state2List s = [s (Left P1),
             s (Left P2),
             s (Left P5),
             s (Left P10),
             s (Right ())]
\end{code}
Obviously, it is useful a function that changes the state of the game for a given object:
\begin{code}
changeState :: Object -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)
\end{code}
Even more useful is a function that changes the state of the game of a list of objects: 
\begin{code}
mChangeState :: [Object] -> State -> State
mChangeState os s = foldr changeState s os
\end{code}
With this, we are now ready to define all the valids plays the adventurers can make for a given state of the game, storing, obviously, the respective duration required and the move made. So, for a given |s :: State|, we'll compute |allValidPlays :: ListLogDur State| $\sim$ |LSD [Duration (String, State)]|. For that, let's think:
\begin{enumerate}
\item We need to move adventurers --- but only adventurers who can pick up the lantern. So, for that given state, we first need to calculate the adventurers who are where the lantern is.
\begin{code}
advsWhereLanternIs :: State -> [Adventurer]
advsWhereLanternIs s = filter ((== s lantern) . s . Left) [P1, P2, P5, P10]
\end{code}
\item Now, since we got the adventurers who can cross, we need to group them into all possible combinations. As we know, a maximum of 2 adventurers can cross. This parametric function
\begin{code}
combinationsUpTo2 :: Eq a => [a] -> [[a]]
combinationsUpTo2 = conc . (split f g) where
      f t = do {x <- t; return [x]}
      g t = do {x <- t; y <- (remove x t); return [x, y]}
      remove x [] = []
      remove x (h:t) = if x==h then t else remove x t
\end{code}
applied to the list of all possivle adventurers will return all possible groups in sublists.
\item Now, we have to get the time both group need to cross --- we just need to map the function |getTimeAdv| and return the maximum value. We may also produce the pair with this result and the initial list of adventurers.
\begin{code}
addTime :: [Adventurer] -> (Int, [Adventurer])
addTime = split (maximum . (map getTimeAdv)) id
\end{code}
\item We also need to add the lantern to the group that is going to cross --- they need the lantern to cross. This returns the list of objects that are going to cross and the time needed to do it.
\begin{code}
addLantern :: (Int, [Adventurer]) -> (Int, [Object])
addLantern = id >< ((lantern :) . map Left)
\end{code}
\item Finally, we need to use the function |map mChangeState| to change the state of that list of lists of objects (which are our possible moves) and encapsulate it in the monad using the composition |LSD . map Duration|. Yes, we are missing something --- the path (or the trace)!!! For now, let's just appreciate the final function. The next subsection will explain how we get the path!
\begin{code}
allValidPlays :: State -> ListLogDur State
allValidPlays s = LSD $ map Duration $ map (id >< (split (toTrace s) id) . (mCS s)) t where
  t = (map (addLantern . addTime) . combinationsUpTo2 . advsWhereLanternIs) s
  mCS = flip mChangeState
  toTrace s s' = printTrace (state2List s, state2List s')
\end{code}

\end{enumerate}

\subsection{The trace log}
As we saw, our monad |ListLogDur| keeps the trace by calling the function |toTrace :: State -> State -> String|. But what does it do?

First, we can see that, according to the representation of the state, adventurers can be represented by indexes. We take advantage of this to be able to present an elegant trace of the moves. For example, if the previous state is |[False, False, False, False, False]| and the current state is |[True, True, False, False, True]|, we know that |P1| and |P2| have crossed (because the first two and the last elements and diferent). So, we can simply compare element to element and, if they are different, we keep the index. In the previous example, it would return |[0,1,4]| --- index 4 represents the lantern, and because we assume that the movements are always valid, we can ignore that.
\begin{code}
index2Adv :: Int -> String
index2Adv 0 = "P1"
index2Adv 1 = "P2"
index2Adv 2 = "P5"
index2Adv 3 = "P10"

indexesWithDifferentValues :: Eq a => ([a], [a]) -> [Int]
indexesWithDifferentValues (l1, l2) = aux l1 l2 0 where
  aux :: Eq a => [a] -> [a] -> Int -> [Int]
  aux [] l _ = []
  aux l [] _ = []
  aux (h1:t1) (h2:t2) index = if h1 /= h2 then index : aux t1 t2 (index + 1)
                             else aux t1 t2 (index + 1)
\end{code}
The result |[0,1,4]| means that \aspas{|P1| and |P2| crosses}. We now have to automate this (pretty) print. We only need to ignore the lantern index (4), convert the indexes to the respective adventurers and define a print function for them.
\begin{code}
printTrace :: ([Bool], [Bool]) -> String
printTrace = prettyLog . (map index2Adv) . init . indexesWithDifferentValues

prettyLog :: [String] -> String
prettyLog = Cp.cond ((>1) . length) f ((++ " cross\n") . head) where
    f = (++" crosses\n") . conc . ((concat . map (++" and ")) >< id) . (split init last)
\end{code}
Let's see the result of applying the function |printTrace| with the previous example.
\begin{verbatim}
 > t = ([False,False,False,False,False],[True,True,False,False,True])
 > printTrace t
 "P1 and P2 crosses\n"
\end{verbatim}
Finnaly, using the function |putStr|, we get a pretty nice log:
\begin{verbatim}
 > putStr $ printTrace t
 P1 and P2 crosses
\end{verbatim}
Back to function |allValidPlays|, we do, for a given state |s| and each following state |s'|,
\begin{code}
toTrace s s' = printTrace (state2List s, state2List s')
\end{code}
So, this representation is done right in the calculation of the possible moves. At the end, we just need to get that already prepared trace. In the next subsection, we'll see the trace of the optimal play which shows how elegant the log is. 
\subsection{Solving the problem}
First, we defined a function that, for a given number |n| and an initial state, calculates
all possible |n|-sequences of moves that the adventures can make. For that, we took advantage of the |do| notation --- let the monad do the work for us!
\begin{code}
exec :: Int -> State -> ListLogDur State
exec 0 s = allValidPlays s
exec n s = do ps <- exec (n-1) s
              allValidPlays ps
\end{code}
The previous functions is nice, but not so much --- we don't know how many sequences are needed to reach the end state. It would be much better if we could execute all possible sequences of moves that the adventures can make for a given state untill it fulfills a predicate over a state (passed as a parameter). Additionally, it also returns the number of moves needed to fulfill that predicate.
\begin{code}
execPred :: (State -> Bool) -> State -> (Int, ListLogDur State)
execPred p s = aux p s 0 where
               aux p s it = let st = exec it s 
                                res = filter pred (map remDur (remLSD st)) in 
                                if length (res) > 0 then ((it+1) , LSD (map Duration res))
                                else aux p s (it+1) where
                                  remDur (Duration a) = a
                                  pred (_, (_,s)) = p s
\end{code}
We may use this version to solve the problem and see who's right. For that, 2 more functions were defined to see if it is possible for all adventurers to be on the other side in |<= n| (and |< n|) minutes and how many moves are needed for that.
\begin{code}
leqX :: Int -> (Int, Bool)
leqX n = if res then (it,res)
                else (0,res) where
                  res = length (filter p (map remDur (remLSD l))) > 0
                  (it,l) = execPred (== gEnd) gInit
                  p (d,(_,_)) = d <= n
                  remDur (Duration a) = a

lX :: Int -> (Int, Bool)
lX n = if res then (it,res)
              else (0,res) where
                res = length (filter p (map remDur (remLSD l))) > 0
                (it,l) = execPred (== gEnd) gInit
                p (d,(_,_)) = d < n
                remDur (Duration a) = a
\end{code}
So let's see who was right!
\begin{itemize}
\item \textit{Is it possible for all adventurers to be on the other side
in |<= 17| minutes and not exceeding 5 moves?}
\begin{code}
leq17 :: Bool
leq17 = p2 (leqX 17) && p1 (leqX 17) <= 5
\end{code}
\begin{verbatim}
 > leq17
 True
\end{verbatim}
\item \textit{Is it possible for all adventurers to be on the other side
in |< 17| minutes?}
\begin{code}
l17 :: Bool
l17 = p2 (lX 17)
\end{code}
\begin{verbatim}
 > l17
 False
\end{verbatim}
\end{itemize}
As we saw, it is possible for all adventurers to be on the other side
in |<= 17| minutes and not exceeding 5 moves. Actually, since |l17| returns |False|, |17| is the the optimal time for solving the problem (with exactly 5 moves). One could also get that information by executing the following function \textit{optimalTrace}, which shows how beautiful our trace log is!
\begin{code}
optimalTrace :: IO ()
optimalTrace =
        putStrLn . t . map remDur . remLSD . p2 $ execPred (== gEnd) gInit where
        t = prt . (split (head . map p1) (map (p1.p2))) . pairFilter . split (minimum . map p1) id
        remDur (Duration a) = a
        pairFilter (d, l) = filter (\(d',(_,_)) -> d == d') l
        p = Cp.cond ((>1) . length) p' (head)
        p' = conc . split (concat . map ((++("\nOR\n\n"))) . init) last
        prt (d, l) = (p l) ++ "\nin " ++ (show d) ++ " minutes."
\end{code}
\begin{verbatim}
 > optimalTrace 
 P1 and P2 crosses
 P1 cross
 P5 and P10 crosses
 P2 cross
 P1 and P2 crosses
 
 OR
 
 P1 and P2 crosses
 P2 cross
 P5 and P10 crosses
 P1 cross
 P1 and P2 crosses
 
 in 17 minutes.
\end{verbatim}
\section{Comparative Analysis and Final Comments}

Let's begin to analyze in terms of scalability. It's easy to see (for the \textsc{Haskell} approach), by running the |exec| function for a very large |n|, that the program slows down! In fact, the runtime is exponential, which one would expect given that it's a brute force implementation. However, UPPAAL model checking engine allows efficient and fast timed automata model exploration. So, for expensive executions, UPPAAL may be a better option. Also, in terms of systems security, modelling in UPPAAL can be easier because of invariants that can be easily defined. The \textsc{Haskell} approach requires the designer to correctly implement the functions to correctly model the problem, fulfilling pre and post conditions and system invariants. Syntax correctness is not enough. Therefore, it is more susceptible to errors.

Even so, UPPAAL have some considerable disadvantages. One of the main disadvantages in UPPAAL is that clocks are logical concepts. The simulator does not allow seeing clock values (only the satisfaction can be checked via constraints). This works if we just want to answer the problem's questions. However, it would be nice to get the time associated to some particular execution (as \textsc{Haskell} does), e.g., the optimal trace duration. In every state of every possible sequence of moves, monad gives us the \aspas{clock value}, i.e. the duration of the moves so far. Also, concerning the space of solutions and taking into account the goal of reaching the final state with the aforementioned criteria, in the \textsc{Haskell} approach, we saw that there were two optimal solutions. However, reachability queries in UPPAAL doesn’t explore the whole state space and stops   
when the given expression becomes true. UPPAAL does not give us all the solutions --- only one, if possible. \textsc{Haskell} gives us the entire space of solutions!

On the other hand, the fact that UPPAAL produces counterexamples allows us to automatically get the trace (or path). After we verify some property (e.g. that is possible for all adventurers to be on the other side in |<= 17| minutes and not exceeding 5 moves), one could negate the property in order to obtain the trace log for that property. In \textsc{Haskell}, there was a need to incorporate the path in the monad definition. Of course, for someone who is modelling with monads in \textsc{Haskell}, that should not be a problem at all.

Considering all this, there's no way to say which approach is better. It all depends on the designer.
A proof of this is that Melânia prefers Uppaal and Paulo prefers \textsc{Haskell}.

\end{document}
