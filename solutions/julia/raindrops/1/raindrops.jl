function raindrops(number::Int)
    drops = []
    number % 3 == 0 && push!(drops, "Pling")
    number % 5 == 0 && push!(drops, "Plang")
    number % 7 == 0 && push!(drops, "Plong")
    size(drops, 1) == 0 ? repr(number) : join(drops)
end