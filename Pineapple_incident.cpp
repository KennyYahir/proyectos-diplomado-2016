#include <iostream>
#include <cstdio>
using namespace std;

int main()
{
	int t, s, x, diff, t2;

	scanf("%d %d %d", &t, &s, &x);

	t2 = t + s;

	diff = x - t2;

	if((diff >= 0 && ((diff-1)%s == 0 || diff % s == 0)) || x == t)
		printf("YES\n");
	else
		printf("NO\n");


	//printf("mmm: %d\n", (diff-1) % s);
	return 0;
}