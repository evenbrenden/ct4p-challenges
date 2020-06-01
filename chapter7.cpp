// 3

#include <functional>

template <class A, class B, class R>
std::function<R (B)> fmap(std::function<A (B)> f, std::function<R (A)> g)
{
    return [f, g](R r) { return f(g(r)); };
}

int main()
{
    return 0;
}
