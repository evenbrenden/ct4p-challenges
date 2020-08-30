// Challenge 5

#include <functional>
#include <utility>

template <class A, class B, class C, class D>
std::pair<B, D> bimap(std::function<A (B)> f, std::function<C (D)> g, std::pair<B, D> pair)
{
    return std::make_pair(f(pair.first), g(pair.second));
}

int main()
{
    return 0;
}
