{-# OPTIONS_GHC -fglasgow-exts #-}
-- | Various Graph Utilities
module Epistem.Util.GraphUtils where {

    import Data.Graph.Inductive;
    import Control.Monad.State;
  
    -- | Function type that takes a graph and a node context and either returns
    --   Nothing, meaning leave the node alone, or Just a list of edges that
    --   are to replace the node (the node is to be removed from the graph)
    type ShortCircuitFn n e = Gr n e -> Context n e -> Maybe [LEdge e];
    
    -- | Apply a short-circuit function to all the nodes in a graph
    shortCircuitNodes :: Gr n e -> ShortCircuitFn n e -> Gr n e;
    shortCircuitNodes gr fn | isEmpty gr = gr
                            | otherwise  = applyGraphFn gr (shortCircuiter fn) 
                                                     $ nodes gr; 
                            
    shortCircuiter :: ShortCircuitFn n e -> Node -> Gr n e -> Gr n e;
    shortCircuiter fn n gr = let (Just ctxt, g2) = match n gr
                              in case fn g2 ctxt of {
                                  Nothing   -> gr;
                                  (Just ee) -> insEdges ee g2;                            
                              }; 
                            
    -- | Take a graph, a function that operates on the graph and apply the func
    --   to the graph to yield a new graph, which is then applied to the next
    --   value
    applyGraphFn :: Gr n e -> (a -> Gr n e  -> Gr n e) -> [a] -> Gr n e;
    applyGraphFn gr _ [] = gr;
    applyGraphFn gr fn (n:nn) = let newGr = fn n gr
                                 in applyGraphFn newGr fn nn;
                                
                                
    -- | Insert a new node before a given one - all incoming edges to the
    --   existing node now go to the new node and the new node points at the
    --   existing node
    insertBefore :: Gr n e  -- original graph
                 -> Node    -- existing node
                 -> n       -- new node
                 -> e       -- edge from new to existing
                 -> Gr n e; -- resulting graph
    insertBefore gr exnode newn edg = 
        let (Just (prec,_,exn,foll),gr2) = match exnode gr
            (low,high) = nodeRange gr2      
         in   insEdge  (high+1, exnode, edg)
            $ delEdges (makeEdgesTo exnode prec)
            $ insEdges (makeLEdgesTo (high+1) prec)
            $ insNode (high+1,newn) gr;
            
    -- | Make labelled edges from adjacents to a given node
    makeLEdgesTo :: Node -> Adj e -> [LEdge e];
    makeLEdgesTo n aa = map (\(e,node)->(node,n,e)) aa;

    -- | Make unlabelled edges from adjacents to a given node
    makeEdgesTo :: Node -> Adj e -> [Edge];
    makeEdgesTo n aa = map (\(e,node)->(node,n)) aa;

    -- | Append a node to a graph (at the next highest node index)
    appendNode :: (Adj e, n, Adj e) -> Gr n e -> Gr n e;
    appendNode (p,n,s) g = (p,(snd (nodeRange g))+1,n,s) & g;

    -- | Check that all nodes are in the given range
    nodesAreInRange :: (Node,Node) -> [Node] -> Bool;
    nodesAreInRange (a,b) [] = True;
    nodesAreInRange (a,b) (n:nn) = (n >= a && n <= b) 
                                   && (nodesAreInRange (a,b) nn);

    -------------------------------------------------------------------
    -- Graph State Monad
    -------------------------------------------------------------------
    type GraphState n e = State (Gr n e);
    
    -- | Get the graph
    getGraph :: GraphState n e (Gr n e);
    getGraph = get;
    
    -- | Set the graph
    putGraph :: Gr n e -> GraphState n e ();
    putGraph = put;
    
    -- | The graph runner
    runGraph :: GraphState n e a -> Gr n e -> Gr n e;
    runGraph gs gr = execState gs gr;
    
    -- | Add a node to the graph and return the new node index
    addNode :: n -> GraphState n e (Node);
    addNode n = do {
        gr <- getGraph;
        let newNode = (snd (nodeRange gr))+1
         in do {
                putGraph $ insNode (newNode,n) gr;
                return newNode
            }
    };    
    
    -- | Apply a function to the graph and store the new one
    withGraph :: (Gr n e -> Gr n e) -> GraphState n e ();
    withGraph fn = do {
        gr <- getGraph;
        putGraph $ fn gr;    
    };

    -- | Apply a function to the graph and return the result
    withGraph' :: (Gr n e -> b -> a) -> b -> GraphState n e a;
    withGraph' fn b = do {
        gr <- getGraph;
        return $ fn gr b;    
    };

    -- | Get a node label from a node
    getNode :: Node -> GraphState n e (n);
    getNode nd = do {
        gr       <- getGraph;
        (Just a) <- return $ lab gr nd;
        return a;
    };
        
    -- | Take a node/edge pair and wire it to the given node
    (==>) :: (ToNode a n e) => (a,e) -> Node -> GraphState n e (Node);
    (==>) (n,e) nd = do {
        node <- toNode n;
        withGraph (insEdge (node,nd,e));
        return node
    };
        
    -- | Redirect all the edges to point at the given node
    redirect :: [LEdge e] -> Node -> GraphState n e ();
    redirect ee a = do {
        addLEdges   $ map (\(p,q,e)->(p,a,e)) ee;
        removeEdges $ map (\(p,q,e)->(p,q))   ee;
    };
    
    -- | Get the incoming edges to the given node
    edgesTo :: Node -> GraphState n e [LEdge e];
    edgesTo = withGraph' inn;
    
    -- | Add edges to the graph
    addLEdges :: [LEdge e] -> GraphState n e ();
    addLEdges = withGraph . insEdges;
    
    -- | Remove edges from the graph
    removeEdges :: [Edge] -> GraphState n e ();
    removeEdges = withGraph . delEdges;
    
    -- | Class of things that can be converted to a Node
    class ToNode a n e where {
        -- | Get a node from the value    
        toNode :: a -> GraphState n e (Node);
    };
    
    -- instance for Node
    instance ToNode Int n e where {
        toNode n = return n;
    };
            
    -- instance for types that are the node label type
    instance ToNode n n e where {
        toNode = addNode;
    };
    
    -------------------------------------------------------------------
    -- Misc utilities..
    -------------------------------------------------------------------

    -- | Remove all but the first and last quotes in a string - in order to
    --   fix a GraphViz string that contains extra quotes from Show String
    removeInternalQuotes :: String -> String;
    removeInternalQuotes [] = [];
    removeInternalQuotes s = let (a,b) = splitAtQuote s
                                 (c,d) = splitAtQuote (reverse b)
                              in if null b then a
                                  else a ++ "\"" ++ (dropQuotes $ reverse d) ++ "\"" ++ c;
    
    splitAtQuote :: String -> (String,String);
    splitAtQuote "" = ("","");
    splitAtQuote ('"':cc) = ("",cc);   
    splitAtQuote (c:cc) = let (a,b) = splitAtQuote cc
                           in (c:a,b);   
                           
    dropQuotes :: String -> String;
    dropQuotes "" = "";
    dropQuotes ('"':cc) = dropQuotes cc;
    dropQuotes (c:cc)   = c : (dropQuotes cc); 
    
}
