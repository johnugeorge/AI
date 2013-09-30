import sys
import signal
import os
import re
from collections import defaultdict
from collections import deque

visited_nodes=defaultdict(list)
node_list=deque()

GOAL_STATE=[1, 2, 3, 8, 0, 4, 7, 6, 5]
#DIRECTIONS=["UP","DOWN","LEFT","RIGHT"]
DIRECTIONS=["RIGHT","LEFT","UP","DOWN"]

def handler(signal, frame):
	print
	print 'You pressed Ctrl+C!..Quiting'
	sys.exit(0)



def general_searchfn(arguments):
	global node_list
	global visited_nodes
	visited_nodes=defaultdict(list)
	node_list=deque()
	nodes_visited = 0
	search_algo=arguments[0]
	initial_state=arguments[1]
	maxLengthOfQueue=0
	depth=1
	if(arguments[2] is not ""):
		depth=int(arguments[2])
	print "Search Algo",search_algo
	#print "initial_state", initial_state
	print "depth", depth
	initial_node=initial_state.split()
	initial_node=[int(n) for n in initial_node]
	node_list.append(initial_node)
	parentStateStr=''.join(str(e) for e in initial_node);
	new_res= [None,None,1]
	visited_nodes[parentStateStr] = new_res
	while(1):
		#search_string=raw_input('Enter your search string here: ')

		#print "Node List",node_list
		if not node_list:
			print "Node list empty. No Solution"
			return 0
		if(maxLengthOfQueue < len(node_list)):
			maxLengthOfQueue =len(node_list)
		first_state=node_list.popleft()
		nodes_visited += 1
		#print "Popped one element " , first_state
		print nodes_visited
		#print first_state , GOAL_STATE
		if(first_state == GOAL_STATE):
			print "Goal state reached"
			return 1
		parentStateStr=''.join(str(e) for e in first_state);
		if(((search_algo == "dls") or (search_algo == "ids"))  and (visited_nodes[parentStateStr][2] >= depth)):
			children=list();
		else:
			children= getChildren(first_state);
		#print "Children",children
		if children:
			queuing_fn(search_algo,first_state,children)

	
def queuing_fn(search_algo,parent,children):
	global node_list
	if(search_algo == "dfs"):
		node_list.extendleft(children)
	elif(search_algo == "bfs"):
		node_list.extend(children)
	elif(search_algo == "dls"):
		node_list.extendleft(children)
	elif(search_algo == "ids"):
		node_list.extendleft(children)
	

	
def getChildren(parent):
	global visited_nodes
	children=list();
	parentStateStr=''.join(str(e) for e in parent);
	for i in range(len(DIRECTIONS)):
		if(isMoveValid(DIRECTIONS[i],parent)):
			newChildState=getNewState(DIRECTIONS[i],parent);
			#print "New Child State ",newChildState, "Direction",DIRECTIONS[i]
			newChildStateStr=''.join(str(e) for e in newChildState);
			if(not isAlreadyVisited(newChildStateStr) ):
				children.append(newChildState)
				new_res= [parentStateStr,DIRECTIONS[i],visited_nodes[parentStateStr][2] + 1]
				visited_nodes[newChildStateStr] = new_res
	#print "parent", parent, "children",children
	return children
			

def isAlreadyVisited(child):
	global visited_nodes
	if not child in visited_nodes:
		return False
	else:
		return True

	
def isMoveValid(op,parent):
	blank_loc=parent.index(0)
	if(op == "UP"):
		if((blank_loc >= 0) and (blank_loc <=2)):
			return False
	elif(op == "DOWN"):
		if((blank_loc >= 6) and (blank_loc <=8)):
			return False
	elif(op == "RIGHT"):
		if((blank_loc %3) == 2):
			return False
	elif(op == "LEFT"):
		if((blank_loc %3) == 0):
			return False
	return True

def getNewState(op, parent):		
	blank_loc=parent.index(0)
	new_loc=None
	child=parent[:]
	if(op == "UP"):
		new_loc=blank_loc-3
	if(op == "DOWN"):
		new_loc=blank_loc+3
	if(op == "RIGHT"):
		new_loc=blank_loc+1
	if(op == "LEFT"):
		new_loc=blank_loc-1
	#print " New Loc" , new_loc," Blank_loc ",blank_loc
	child[blank_loc],child[new_loc]=child[new_loc],child[blank_loc]
	return child



def parse(arg):
	match = re.search(r'\((\w+)\s*\'\(([\d\s]+)\)[\'\s]*([\w\d\s]*)', arg)
	if match:
		ret = (match.group(1) ,match.group(2),match.group(3))
		return ret 
	else:
		print "Error in parsing the arguments. Verify the format"
        
def searchfn(arguments):
	search_algo=arguments[0]
	initial_state=arguments[1]
	if(search_algo == "ids"):
		depth=0
		while(1):
			depth += 1
			ret = general_searchfn((search_algo,initial_state,depth))
			if ( ret == 1 ):
				print "Solution found"
				break
	elif(search_algo == 'greedy' or search_algo == 'astar'):
		ret=heuristic_searchfn(arguments)
	elif(search_algo == 'idastar'):
		ret=idastar_searchfn(arguments)
	else:
		ret=general_searchfn(arguments)
	if ( ret == 1 ):
		print "Solution found"
		return 1
	else:
		print "No solution found"
		sys.exit()

def idastar_searchfn(arguments):
	global node_list
	global visited_nodes
	#visited_nodes=defaultdict(list)
	#node_list=deque()
	maxLengthOfQueue=0
	nodes_visited = 0
	search_algo=arguments[0]
	initial_state=arguments[1]
	if(arguments[2] is not ""):
		heuristicFn=arguments[2]
	else:
		print "Error in getting heuristic"
		sys.exit()
	print "Search Algo",search_algo
	#print "initial_state", initial_state
	print "HeuristicFn", heuristicFn
	initial_node=initial_state.split()
	initial_node=[int(n) for n in initial_node]
	node_list.append((initial_node,0,1))
	parentStateStr=''.join(str(e) for e in initial_node);
	new_res= [None,None,1]
	#visited_nodes[parentStateStr] = new_res
	root=node_list[0][0]
	fLimit=fCost(root,heuristicFn,1)
	while(1):
		root=node_list[0][0]
		visited_nodes=defaultdict(list)
		visited_nodes[parentStateStr] = new_res
		(solution,fLimit)=DfsContour(root,fLimit,heuristicFn)
		if(solution is not None):
			print "Goal state reached"
			return 1
		if(fLimit == sys.maxint):
			print "No Solution Found"
			return 0

def DfsContour(state,fLimit,heuristicFn):
	#raw_input()
	stateStr=''.join(str(e) for e in state)
	totalCost= fCost(state,heuristicFn, visited_nodes[stateStr][2])
	#print " Flimit ",fLimit," totalCost ",totalCost
        #print state
	if(totalCost > fLimit):
		return (None,totalCost)
	if(state == GOAL_STATE):
		#print "Solution Found"
		return (GOAL_STATE,fLimit)
	minVal=sys.maxint
        children=getChildrenHeuristic(state,heuristicFn)
	#print "state ",state , "Children ",children
	#print "Visited nodes" ,visited_nodes
	for child in children:
		#print "state ",state , "Child",child ," flimit ", fLimit, "TotalCost ",totalCost
		childStr = ''.join(str(e) for e in child[0])
		fLimitChild=fCost(child[0],heuristicFn,visited_nodes[childStr][2])
		(solution,fVal)=DfsContour(child[0],fLimit,heuristicFn)
		if(solution is not None):
			return (solution,fLimit)
		if(fVal < minVal):
			minVal=fVal
        return (None,minVal) 
	
        


def fCost(state,heuristicFn,actualCost):
	heuristicVal= findHeuristicVal(state,heuristicFn)
	totalCost=heuristicVal + actualCost
	return totalCost

def heuristic_searchfn(arguments):
	global node_list
	global visited_nodes
	visited_nodes=defaultdict(list)
	node_list=deque()
	maxLengthOfQueue=0
	nodes_visited = 0
	search_algo=arguments[0]
	initial_state=arguments[1]
	if(arguments[2] is not ""):
		heuristicFn=arguments[2]
	else:
		print "Error in getting heuristic"
		sys.exit()
	print "Search Algo",search_algo
	#print "initial_state", initial_state
	print "HeuristicFn", heuristicFn
	initial_node=initial_state.split()
	initial_node=[int(n) for n in initial_node]
	node_list.append((initial_node,0,1))
	parentStateStr=''.join(str(e) for e in initial_node);
	new_res= [None,None,1]
	visited_nodes[parentStateStr] = new_res
	while(1):
		#search_string=raw_input('Enter your search string here: ')

		#print "Node List",node_list
		if not node_list:
			print "Node list empty. No Solution"
			return 0
		if(maxLengthOfQueue < len(node_list)):
			maxLengthOfQueue =len(node_list)
		if(search_algo == "greedy"):
			node_list=deque(sorted(node_list,key=lambda tup: tup[1]))
		elif(search_algo == "astar"):
			node_list=deque(sorted(node_list,key=lambda tup: tup[1]+tup[2]))
		else:
			print "Error search algo"
			sys.exit()
		first_state=node_list.popleft()
		nodes_visited += 1
		#print "Popped one element " , first_state
		print "Nodes visited ",nodes_visited
		print "MaxLength of Queue",maxLengthOfQueue
		#print "Present State ",first_state , "Goal State ",GOAL_STATE
		if(first_state[0] == GOAL_STATE):
			print "Goal state reached"
			return 1
		parentStateStr=''.join(str(e) for e in first_state[0]);
		children= getChildrenHeuristic(first_state[0],heuristicFn);
		#print "Children",children
		if children:
			node_list.extend(children)


def getChildrenHeuristic(parent,heuristicFn):
	global visited_nodes
	children=list();
	#print "Parent ",parent
	parentStateStr=''.join(str(e) for e in parent);
	for i in range(len(DIRECTIONS)):
		if(isMoveValid(DIRECTIONS[i],parent)):
			newChildState=getNewState(DIRECTIONS[i],parent);
			heuristicVal=findHeuristicVal(newChildState,heuristicFn)
			#print "New Child State ",newChildState, "Direction",DIRECTIONS[i]
			newChildStateStr=''.join(str(e) for e in newChildState);
			if(not isAlreadyVisited(newChildStateStr) ):
				children.append((newChildState,heuristicVal,visited_nodes[parentStateStr][2] + 1))
				new_res= [parentStateStr,DIRECTIONS[i],visited_nodes[parentStateStr][2] + 1]
				visited_nodes[newChildStateStr] = new_res
	#print "parent", parent, "children",children
	return children
			

def findHeuristicVal(state,heuristicFn):
	heuristic_val=0
	if(heuristicFn == "h1"):
		for i in state:
			if(state[i] != GOAL_STATE[i] and state[i] != 0):
				heuristic_val = heuristic_val +1

	elif(heuristicFn == "h2"):
		for elem in state:
			if(elem !=0):
				currentPos=state.index(elem)
				finalPos=GOAL_STATE.index(elem)
				#print "Elem ",elem," currentPos ",currentPos, "finalPos ",finalPos
				while((currentPos/3 - finalPos/3) != 0):
					if(currentPos > finalPos):
						currentPos -= 3
					elif(currentPos < finalPos):
						currentPos += 3
					heuristic_val +=1
				heuristic_val += abs(currentPos - finalPos)
				#print "Elem ",elem," heuristic_val ",heuristic_val
		#print "state ",state ,"Heuristic Val ",heuristic_val
		return heuristic_val



	else:
		print "No defined Heuristic fn ", heuristicFn
		sys.exit()
	#print "heuristic_val " , heuristic_val
	return heuristic_val

def printPath():
	goalStateStr=''.join(str(e) for e in GOAL_STATE);
        temp=goalStateStr
	directions=[]
	while(visited_nodes[temp][0] != None):
		#print " directions ", visited_nodes[temp][1]
		directions.append(visited_nodes[temp][1])
                temp= visited_nodes[temp][0]
	print "======Path from Input to Goal==========="
	print "Total Movements",len(directions)
	if(len(directions) > 50):
		print "Not printing directions as total moves needed > 50"
		return
	for i in reversed(directions):
		sys.stdout.write(i+" ")
	print

def main():
	print "Main Function"
	signal.signal(signal.SIGINT, handler)
	if len(sys.argv) !=2:
		print " Wrong Arguments"
		exit(0)
	arg=sys.argv[1]
	#general_searchfn(parse(arg));
	if(searchfn(parse(arg)) == 1):
		printPath()


if __name__ == '__main__':
	main()

