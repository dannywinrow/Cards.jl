module Cards

export Suit, Rank, Card, Hand, ♣, ♢, ♡, ♠, .., deal!, points, suitcount, rankcount, suitbinary, shuffle, shuffled_deck, rand

import Base: *, |, &, ~, -, parse, isless

using Random: randperm
import Random
using StatsBase: sample

"""
Encode a suit as a 2-bit value (low bits of a `UInt8`):

- 0 = ♣ (clubs)
- 1 = ♢ (diamonds)
- 2 = ♡ (hearts)
- 3 = ♠ (spades)

The suits have global constant bindings: `♣`, `♢`, `♡`, `♠`.
"""
struct Suit
    i::UInt8
    Suit(s::Integer) = 0 ≤ s ≤ 3 ? new(s) :
        throw(ArgumentError("invalid suit number: $s"))
end

char(s::Suit) = Char(0x2663-s.i)
Base.string(s::Suit) = string(char(s))
Base.show(io::IO, s::Suit) = print(io, char(s))
Base.isless(c::Suit,d::Suit) = c.i < d.i

Suit(char::Char) = Suit(suit_map[char])

const ♣ = Suit(0)
const ♢ = Suit(1)
const ♡ = Suit(2)
const ♠ = Suit(3)

const suits = [♣, ♢, ♡, ♠]

"""
Encode a rank as a 6-bit value (low bits of a `UInt8`):

Ranks are assigned as follows:

- numbered cards (2 to 10) have rank equal to their number
- jacks, queens and kings have ranks 11, 12 and 13
- there are low and high aces with ranks 1 and 14
- there are low and high jokers with ranks 0 and 15

This allows any of the standard orderings of cards ranks to be
achieved simply by choosing which aces or which jokers to use.
"""
struct Rank
    i::UInt8
    Rank(s::Integer) = 0 ≤ s ≤ 15 ? new(s) :
        throw(ArgumentError("invalid rank number: $s"))
end
Base.isless(c::Rank,d::Rank) = c.i < d.i

Rank(char::Char) = Rank(rank_map[char])

function char(r::Rank)
    1 ≤ r.i ≤ 14 && return "123456789TJQKA"[r.i]
    '\U1f0cf'
end
Base.string(r::Rank) = string(char(r))
Base.show(io::IO, r::Rank) = print(io, char(r))



"""
Encode a playing card as a 6-bit integer (low bits of a `UInt8`):

- low bits represent rank from 0 to 15
- high bits represent suit (♣, ♢, ♡ or ♠)

Ranks are assigned as follows:

- numbered cards (2 to 10) have rank equal to their number
- jacks, queens and kings have ranks 11, 12 and 13
- there are low and high aces with ranks 1 and 14
- there are low and high jokers with ranks 0 and 15

This allows any of the standard orderings of cards ranks to be
achieved simply by choosing which aces or which jokers to use.
There are a total of 64 possible card values with this scheme,
represented by `UInt8` values `0x00` through `0x3f`.
"""
struct Card
    value::UInt8
end

function Card(r::Integer, s::Integer)
    0 ≤ r ≤ 15 || throw(ArgumentError("invalid card rank: $r"))
    return Card(((s << 4) % UInt8) | (r % UInt8))
end
Card(r::Int, s::Suit) = Card(r, s.i)
Card(r::Rank, s::Suit) = Card(r.i, s.i)
Card(card::AbstractString) = parse(Card,card)

Base.parse(::Type{Card},card::AbstractString) = Card(Rank(card[1]),Suit(card[2]))

function Base.isless(c::Card,d::Card)
    rank(c) < rank(d) && return true
    rank(c) > rank(d) && return false
    suit(c) < suit(d)
end

#parsing maps
const rank_map = Dict(
    '2'=> 2, '3'=> 3, '4'=> 4, '5'=> 5, '6'=> 6, '7'=> 7, '8'=> 8, '9'=> 9,
    'T'=> 10, 'J'=> 11, 'Q'=> 12, 'K'=> 13, 'A'=> 14
    )
const suit_map = Dict('c'=> 0, 'd'=> 1, 'h'=> 2, 's'=> 3)

const suit_reverse_map = Dict(0 => 'c', 1 => 'd', 2 => 'h', 3 => 's')

rankchar(c::Card) = c |> rank |> char
suit(c::Card) = Suit((0x30 & c.value) >>> 4) 
rank(c::Card) = Rank((c.value & 0x0f) % Int8)
value(c::Card) = c.value
Base.show(io::IO, c::Card) = print(io, rank(c), suit(c))
#rand(::Type{Hand},ncards=2) = Hand(sample)
*(r::Rank, s::Suit) = Card(r, s)
*(r::Int, s::Suit) = Card(r, s)
*(s::Suit, r::Rank) = Card(r, s)

for s in "♣♢♡♠", (r,f) in zip(10:14, "TJQKA")
    ss, sc = Symbol(s), Symbol("$f$s")
    @eval (export $sc; const $sc = Card($r,$ss))
end

"""
Represent a hand (set) of cards using a `UInt64` bit set.
"""
struct Hand <: AbstractSet{Card}
    cards::UInt64
    Hand(cards::UInt64) = new(cards)
end

bit(c::Card) = one(UInt64) << c.value
bits(s::Suit) = UInt64(0xffff) << 16(s.i)
bits(r::UInt8) = UInt64(0x0001000100010001) << r
bits(r::Rank) = UInt64(0x0001000100010001) << r.i

function Hand(cards)
    hand = Hand(zero(UInt64))
    for card in cards
        card isa Card || throw(ArgumentError("not a card: $(repr(card))"))
        i = bit(card)
        hand.cards & i == 0 || throw(ArgumentError("duplicate cards are not supported"))
        hand = Hand(hand.cards | i)
    end
    return hand
end
Hand(hand::Union{String,SubString}) = Hand(Card(card.match) for card in eachmatch(r"[\dTJQKA][cdhs]",hand))

#Base.parse(Hand,hand::String) = Hand(parse(Card,card.match) for card in eachmatch(r"[\dTJQKA][cdhs]",hand))
stringhand(hand::Hand) = join("A23456789TJQKA"[rank(card).i]*suit_reverse_map[suit(card).i] for card in hand)

Base.in(c::Card, h::Hand) = (bit(c) & h.cards) != 0
Base.length(h::Hand) = count_ones(h.cards)
Base.isempty(h::Hand) = h.cards == 0
Base.lastindex(h::Hand) = length(h)

function Base.iterate(h::Hand, s::UInt8 = trailing_zeros(h.cards) % UInt8)
    (h.cards >>> s) == 0 && return nothing
    c = Card(s); s += true
    c, s + trailing_zeros(h.cards >>> s) % UInt8
end

function Base.unsafe_getindex(h::Hand, i::UInt8)
    card, s = 0x0, 0x5
    while true
        mask = 0xffff_ffff_ffff_ffff >> (0x40 - (0x1<<s) - card)
        card += UInt8(i > count_ones(h.cards & mask) % UInt8) << s
        s > 0 || break
        s -= 0x1
    end
    return Card(card)
end
Base.unsafe_getindex(h::Hand, i::Integer) = Base.unsafe_getindex(h, i % UInt8)

function Base.getindex(h::Hand, i::Integer)
    @boundscheck 1 ≤ i ≤ length(h) || throw(BoundsError(h,i))
    return Base.unsafe_getindex(h, i)
end

function Base.show(io::IO, hand::Hand)
    cards = sort([c for c in hand],by=value,rev=true)
    for c in cards
        print(io,c)
        print(io," ")
    end
end
Base.show(io::IO, ::MIME"text/plain", hand::Hand) = print(io, hand)

~(a::Hand) = Hand(~a.cards)

a::Hand - b::Hand = Hand(a.cards & ~b.cards)
a::Hand - c::Card = Hand(a.cards & ~bit(c))

a::Hand | b::Hand = Hand(a.cards | b.cards)
a::Hand | c::Card = Hand(a.cards | bit(c))
c::Card | h::Hand = h | c

a::Hand & b::Hand = Hand(a.cards & b.cards)
h::Hand & s::Suit = Hand(h.cards & bits(s))
s::Suit & h::Hand = h & s

Base.intersect(s::Suit, h::Hand) = h & s
Base.intersect(h::Hand, s::Suit) = intersect(s::Suit, h::Hand) 

*(rr::OrdinalRange{<:Integer}, s::Suit) = Hand(Card(r,s) for r in rr)
..(r::Integer, c::Card) = (r:rank(c))*suit(c)
..(a::Card, b::Card) = suit(a) == suit(b) ? rank(a)..b :
    throw(ArgumentError("card ranges need matching suits: $a vs $b"))

const deck = Hand(Card(r,s) for s in suits for r = 2:14)
const deckarr = collect(deck)

Random.rand(::Type{Hand},ncards::Integer=4)=Hand(sample(deckarr,ncards;replace=false))
function Random.rand(::Type{Hand},ncards::Vector{<:Integer})
    arr = sample(deckarr,sum(ncards);replace=false)
    v = Vector{Hand}(undef,length(ncards))
    t=0
    for i in eachindex(ncards)
        y = t+1
        t += ncards[i]
        v[i] = Hand(arr[y:t])
    end
    v
    #[(y=t+1;t+=n;Hand(arr[y:t])) for n in ncards]
end

shuffled_deck() = shuffle([Card(r,s) for s in suits for r = 2:14])

function shuffle(deck)
    deck[randperm(length(deck))]
end

function deal!(deck,n)
    hand = Hand(pop!(deck) for _ in 1:n)
end

Base.empty(::Type{Hand}) = Hand(zero(UInt64))

suitbinary(hand::Hand,suit::Suit) = suitbinary(hand,suit.i)
suitbinary(hand::Hand,i::Int) = (hand.cards >> (16 * i + 2)) & 0x0000_0000_0000_1fff

suitcount(hand::Hand) = [count_ones((hand.cards >> (16*s)) & 0xffff) for s in 0:3]
rankcount(hand::Hand) = [count_ones(hand.cards & (0x0004_0004_0004_0004 << s)) for s in 0:12]

#@eval Base.rand(::Type{Hand}) = Hand($(deck.cards) & rand(UInt64))

function deal!(counts::Vector{<:Integer}, hands::AbstractArray{Hand}, offset::Int=0)
    for rank = 2:14, suit = 0:3
        while true
            hand = rand(1:4)
            if counts[hand] > 0
                counts[hand] -= 1
                hands[offset + hand] |= Card(rank, suit)
                break
            end
        end
    end
    return hands
end

deal() = deal!(fill(13, 4), fill(empty(Hand), 4))

function deal(n::Int)
    counts = fill(0x0, 4)
    hands = fill(empty(Hand), 4, n)
    for i = 1:n
        deal!(fill!(counts, 13), hands, 4(i-1))
    end
    return permutedims(hands)
end

function points(hand::Hand)
    p = 0
    for rank = 11:14, suit = 0:3
        card = Card(rank, suit)
        p += (rank-10)*(card in hand)
    end
    return p
end

end # Cards