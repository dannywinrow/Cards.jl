using BenchmarkTools


@benchmark Cards.card_map[c] setup = (c = rand("23456789TJQKA")*rand("cdhs"))

@benchmark Rank(c[1])*Suit(c[2]) setup = (c = rand("23456789TJQKA")*rand("cdhs"))

allomahahands = collect(AnyCard())