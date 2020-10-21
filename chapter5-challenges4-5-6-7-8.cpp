#include <cassert>

enum class Which { Left, Right };

template <class A, class B>
class Either {
public:
    Either(A left)
    : left_(left)
    , which_(Which::Left)
    {}
    Either(B right)
    : right_(right)
    , which_(Which::Right)
    {}
    A left() const
    {
        assert(which_ == Which::Left);
        return left_;
    };
    B right() const
    {
        assert(which_ == Which::Right);
        return right_;
    };
    Which which() const { return which_; }
private:
    A left_;
    B right_;
    Which which_;
};

int i(int n)
{
    return n;
}

int j(bool b)
{
    return b ? 0: 1;
}

int m(Either<int, bool> const & e)
{
    if (e.which() == Which::Left)
        return e.left();
    return 0;
}

int main()
{
    // Implement the equivalent of Haskell Either as a generic type in your
    // favorite language (other than Haskell).
    {
        Either<int, bool> leftie = Either<int, bool>(4);
        Either<int, bool> rightie = Either<int, bool>(true);
    }

    // Show that Either is a "better" coproduct than int equipped with two
    // injections (...)
    {
        int i_input_int = 4;
        Either<int ,bool> i_input_either = Either<int, bool>(i_input_int);
        int factorized_i = m(i_input_either);

        bool j_input_bool = true;
        Either<int ,bool> j_input_either = Either<int, bool>(j_input_bool);
        int factorized_j = m(j_input_either);

    }

    // How would you argue that int with the two injections i and j cannot be
    // "better" than Either?

    // We have already shown the Either is better by showing that left()/right()
    // can be factorized by m() through i()/j(). To show that the opposite is
    // not true, imagine a function n: Either n(int). n cannot take a bool, so
    // it can not factorize j(). For example, n(rightie.right()) does not work
    // (bar coercion). Therefore, int is not better than Either. This is
    // an argument similar to what was done with Int for products in the book.

    // What about these injections? (...)

    // Using that i(), we can not factorize m(MAX_INT) and m(MAX_INT - 1), so
    // that is not better either (cheap pun but i need a comic relief rn).

    return 0;
}

// Come up with an inferior candidate for a coproduct of int and bool that
// cannot be better than Either because it allows multiple acceptable morphisms
// from it to Either.

template <class A, class B>
class InferiorEither {
public:
    InferiorEither(A left)
    : left1_(left)
    , which_(Which::Left)
    {}
    InferiorEither(B right)
    : right2_(right)
    , which_(Which::Right)
    {}
    A left() const
    {
        assert(which_ == Which::Left);
        return left2_;
    };
    B right() const
    {
        assert(which_ == Which::Right);
        return right1_;
    };
    Which which() const { return which_; }
private:
    A left1_;
    A left2_;
    B right1_;
    B right2_;
    Which which_;
};

// This is inferior because you are free to choose which of two ints and two
// bools you want to use, i.e. there are multiple morphisms from it to Either.
// This is an argument similar to what was done with (Int, Int, Bool) for
// products in the book.
