int main() {
    int month = 3;

    switch (month) {
    case 12:
        printf("December");
        break;
    case 1:
        printf("January");
        break;
    default:
        printf("Other Month");
    }

    while (month < 6) {
        printf("Month");
        month++;
    }

    do {
        printf("Month");
        month++;
    } while (month < 9);

    if (month >= 9) {
        printf("Month is greater than or equal to 9");
    }

    int count = 0;
    while (count < 3) {
        printf("Count");
        count++;
    }

    for (int i = 0; i < 3; i++) {
        printf("i");
    }

    int extra = 0;
    while (extra < 6) {
        printf("Extra");
        extra++;
    }

    do {
        printf("Extra");
        extra++;
    } while (extra < 9);

    if (extra >= 9) {
        printf("Extra is greater than or equal to 9");
    }

    for (int j = 0; j < 3; j++) {
        printf("Loop");
    }

    int k = 0;
    while (k < 2) {
        printf("While Loop");
        k++;
    }

    return 0;
}