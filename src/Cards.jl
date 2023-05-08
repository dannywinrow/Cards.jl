module Cards

export Suit, Card, Hand, ♣, ♢, ♡, ♠, .., deal, points

import Base: *, |, &

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

const ♣ = Suit(0)
const ♢ = Suit(1)
const ♡ = Suit(2)
const ♠ = Suit(3)

const suits = [♣, ♢, ♡, ♠]

#parsing maps
const rank_map = Dict(
    '2'=> 2, '3'=> 3, '4'=> 4, '5'=> 5, '6'=> 6, '7'=> 7, '8'=> 8, '9'=> 9,
    'T'=> 10, 'J'=> 11, 'Q'=> 12, 'K'=> 13, 'A'=> 14
    )
const suit_map = Dict('c'=> 0, 'd'=> 1, 'h'=> 2, 's'=> 3)
const suit_reverse_map = Dict(0 => 'c', 1 => 'd', 2 => 'h', 3 => 's')
const rank_reverse_map = Dict(value => key for (key, value) in rank_map)

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
Card(r::Integer, s::Suit) = Card(r, s.i)
Card(card::Union{String,SubString}) = Card(rank_map[card[1]],Suit(suit_map[card[2]]))
#Can't import Base.parse for some reason as then 'using .Cards' hangs
#Base.parse(Card,card::String) = Card(rank_map[card[1]],Suit(suit_map[card[2]]))


suit(c::Card) = Suit((0x30 & c.value) >>> 4)
rank(c::Card) = (c.value & 0x0f) % Int8


function Base.show(io::IO, c::Card)
    r = rank(c)
    if 1 ≤ r ≤ 14
        print(io, "123456789TJQKA"[r])
    else
        print(io, '\U1f0cf')
    end
    print(io, suit(c))
end

*(r::Integer, s::Suit) = Card(r, s)

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

function Hand(cards)
    hand = Hand(zero(UInt64))
    for card in cards
        card isa Card || throw(ArgumentError("not a card: $repr(card)"))
        i = bit(card)
        hand.cards & i == 0 || throw(ArgumentError("duplicate cards are not supported"))
        hand = Hand(hand.cards | i)
    end
    return hand
end
Hand(hand::Union{String,SubString}) = Hand(Card(card.match) for card in eachmatch(r"[\dTJQKA][cdhs]",hand))

#Base.parse(Hand,hand::String) = Hand(parse(Card,card.match) for card in eachmatch(r"[\dTJQKA][cdhs]",hand))
stringhand(hand::Hand) = join(rank_reverse_map[rank(card)]*suit_reverse_map[suit(card)] for card in hand)

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
    if isempty(hand) || !get(io, :compact, false)
        print(io, "Hand([")
        for card in hand
            print(io, card)
            (bit(card) << 1) ≤ hand.cards && print(io, ", ")
        end
        print(io, "])")
    else
        for suit in suits
            s = hand & suit
            isempty(s) && continue
            show(io, suit)
            for card in s
                r = rank(card)
                if r == 10
                    print(io, '\u2491')
                elseif 1 ≤ r ≤ 14
                    print(io, "123456789TJQKA"[r])
                else
                    print(io, '\U1f0cf')
                end
            end
        end
    end
end

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

Base.empty(::Type{Hand}) = Hand(zero(UInt64))

@eval Base.rand(::Type{Hand}) = Hand($(deck.cards) & rand(UInt64))

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