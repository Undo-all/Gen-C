#include <stdio.h>
int times2(int);
int main();

int times2(int x) {
    return (2) * (x);
}

int main() {
    printf("%d\n", times2(1));
    printf("%d\n", times2(2));
    return 0;
}

