#include <functional>
#include <cassert>

template <class T>
T id(T x)
{
    return x;
}

template <class A, class B, class C>
std::function<A (C)> compose(
    std::function<A (B)> f,
    std::function<B (C)> g)
{
    return [f, g](A a) { return g(f(a)); };
}

int main()
{
    std::function<int (int)> i = [](int x) { return id(x); };
    std::function<int (int)> f = [](int x) { return x * x; };
    auto fid = compose(f, i);
    auto idf = compose(i, f);
    assert(fid(4) == idf(4));
    return 0;
}
