int main() {
    int day = 4;    
    switch (day) {
    case 6:
        printf("Today is Saturday");
        break;
    case 7:
        printf("Today is Sunday");
        break;
    default:
        printf("Looking forward to the Weekend");
    }
    while (day < 7) {
        printf("Day");
        day++;
    }
    do {
        printf("Day");
        day++;
    } while (day < 7);
    if (day >= 7 || day == 7) {
        printf("Today is Sunday");
    }
}