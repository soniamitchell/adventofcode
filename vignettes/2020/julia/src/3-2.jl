# Determine the number of trees you would encounter if, for each of the following 
# slopes, you start at the top-left corner and traverse the map all the way to the 
# bottom
iterate = [[1,1], [3,1], [5,1], [7,1], [1,2]];
results = [trees(iterate[x][1], iterate[x][2], dat) for x in eachindex(iterate)];

# What do you get if you multiply together the number of trees encountered on each of  
# the listed slopes?
prod(results)
