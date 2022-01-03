# What is the product of the three entries that sum to 2020?
numbers = [x for x in combinations(dat, 3) if sum(x) == 2020];
numbers = reshape(numbers)[1];
res = prod(Int, numbers) 
