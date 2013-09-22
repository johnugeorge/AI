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
	depth=1
	if(arguments[2] is not ""):
		depth=int(arguments[2])
	print "Search Algo",search_algo
	print "initial_state", initial_state
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
				new_res= [parent,DIRECTIONS[i],visited_nodes[parentStateStr][2] + 1]
				visited_nodes[newChildStateStr] = new_res
	#print "parent", parent, "children",children
	return children
			

def isAlreadyVisited(child):
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
	match = re.search(r'\((\w+)\s*\'\(([\d\s]+)\)([\d\s]*)', arg)
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
	else:
		general_searchfn(arguments)

def main():
	print "Main Function"
	signal.signal(signal.SIGINT, handler)
	if len(sys.argv) !=2:
		print " Wrong Arguments"
		exit(0)
	arg=sys.argv[1]
	#general_searchfn(parse(arg));
	searchfn(parse(arg));


if __name__ == '__main__':
	main()

