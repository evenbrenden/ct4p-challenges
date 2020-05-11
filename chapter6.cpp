// 2-4

class Shape {
public:
    virtual float area() = 0;
    virtual float circ() = 0;
};

const float pi = 3.14;

class Rect : public Shape {
public:
    Rect();
    float area() override
    {
        return d * h;
    };
    float circ() override
    {
        return 2 * pi * (d + h);
    };
    float d;
    float h;
};

class Circle : public Shape {
public:
    Circle();
    float area() override
    {
        return pi * r * r;
    };
    float circ() override
    {
        return 2 * pi * r;
    };
    float r;
};

class Square : public Shape {
public:
    Square();
    float area() override
    {
        return d * d;
    };
    float circ() override
    {
        return 2 * pi * (d + d);
    };
    float d;
};

// main

int main()
{
    return 0;
}
