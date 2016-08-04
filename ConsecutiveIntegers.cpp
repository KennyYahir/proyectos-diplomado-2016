#include <iostream>
#include <cstdio>
#include <cmath>
using namespace std;


int main()
{
	long long n, coc, diff, k;

	while(cin >> n && n != -1)
	{

		for(int i = 1; (i*(i+1) / 2) <= n; i++)
		{
			diff = n - (i*(i+1) / 2);

			if(diff % i == 0){
				coc = diff / i;
				k = i;
			}
		}

		printf("%d = %d + ... + %d\n", n, 1 + coc, k + coc);
	}
	return 0;
}