package fpinscala.monoids.isomorphism

import fpinscala.monoids.Monoid.{booleanAnd, booleanOr}
import fpinscala.monoids.OrderTwoMonoids.nonGroupMonoid

// is it possible to check for isomorphism?
// I happen to know that `booleanAnd`, `booleanOr`, and `nonGroupMonoid`
// are all isomorphic to each other
// in fact `booleanAnd` and `booleanOr` are isomorphic via the negation function (!)
// that is to say that both homomorphisms are:
// def homomorphism(x: Boolean): boolean = !x
// I'm kind of thinking since we're dealing with finite monoids
// it's kind of reasonable for me to expect the user to have knowledge of their monoids
// so I'm gonna ask them to specify the elements of the monoid as well as the monoid

// Once I have that info I guess all I need to do is compute the Caley table for each
// And then somehow check the they have the same structure
// A nice thing about this approach is that you don't actually need to know the homomorphisms
// to detect the isomorphism
// I feel like sometimes these homomorphisms might be a bit mad and hard to find

// I can't really think of any practical benefits of doing this exercise at the moment
// but I've decided I don't care

// In physics I basically always used matrices to represent groups
// be it the lorentz group, or just basic rotations
// but I want this calculation to work for finite monoids of Any type
// A fascinating test case would be what in my head I call the "Mobius generators"
// the six functions from Part II maths that formed a group under function composition
// And I think they ended up being isomorphic to the symmetry group of a triangle or something
object IsomorphismDetection {

}