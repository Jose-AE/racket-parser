int main() {
    int grade = 75;

    switch (grade) {
    case 90:
        printf("A");
        break;
    case 80:
        printf("B");
        break;
    default:
        printf("C or below");
    }

    while (grade < 80) {
        printf("Grade");
        grade++;
    }

    do {
        printf("Grade");
        grade++;
    } while (grade < 85);

    if (grade >= 85) {
        printf("Grade is greater than or equal to 85");
    }

    int count = 0;
    while (count < 3) {
        printf("Count");
        count++;
    }

    for (int i = 0; i < 3; i++) {
        printf("i");
    }

    int a = 0;
    while (a < 5) {
        if (a == 2) {
            a++;
            continue;
        }
        printf("a");
        a++;
    }

    int b = 0;
    do {
        if (b == 1) {
            b++;
            continue;
        }
        printf("b");
        b++;
    } while (b < 5);

    int x = 0;
    while (x < 7) {
        printf("x");
        x++;
    }

    for (int y = 0; y < 7; y++) {
        printf("y");
    }

    int z = 0;
    while (z < 8) {
        printf("z");
        z++;
    }

    for (int m = 0; m < 8; m++) {
        printf("m");
    }

    return 0;
}