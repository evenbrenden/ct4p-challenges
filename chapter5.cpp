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
    A left()
    {
        assert(which_ == Which::Left);
        return left_;
    };
    B right()
    {
        assert(which_ == Which::Right);
        return right_;
    };
    Which which() { return which_; }
private:
    A left_;
    B right_;
    Which which_;
};

int main()
{
    Either<int, bool> leftie = Either<int, bool>(4);
    Either<int, bool> rightie = Either<int, bool>(true);

    return 0;
}
