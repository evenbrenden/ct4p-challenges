#include <functional>
#include <map>

template <class A, class B>
B memoize(std::function<A (B)> f, A a)
{
    static std::map<A, B> memos;

    if (memos.find(a) != memos.end()) {
        printf("Found memoized value\n");
        return memos[a];
    }

    printf("Memoized new value\n");
    return memos[f(a)];
}

int main()
{
    std::function<int (int)> f = [](int x) { return x * x; };

    memoize(f, 1);
    memoize(f, 2);
    memoize(f, 1);

    return 0;
}
