#include <cstdio>
#include <iostream>
#include <vector>
#define For(x,a,b) for(int x = a; x<b; x++)
using namespace std;

int main()
{
	int n, arr[200], suma = 0;
	bool usado[200];

	while(cin >> n){
		
		suma = 0;

		For(i,0,n){
			cin >> arr[i];
			suma += arr[i];
			usado[i] = false;
		}

		int x = suma / (n / 2);


		For(i,0,n){
			For(j,i+1,n){
				if(usado[i] == false and usado[j] == false and arr[i] + arr[j] == x){
					usado[j] = true, usado[i] = true;
					printf("%d %d\n",i+1,j+1);
				}
			}
		}
	}
	

	return 0;
}