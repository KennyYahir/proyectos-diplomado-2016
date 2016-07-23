#include <cstdio>
#include <iostream>
#include <vector>
#include <cstring>
#include <set>
#define For(x,a,b) for(int x = a; x<b; x++)
using namespace std;


int main()
{
	int n, m, tot, a, b, nc, nr;
	//bool rows[100*1000 + 5], cols[100*1000 + 5];
	set <int> rows;
	set <int> cols;

	while(cin >> n >> m)
	{
		//memset(rows, 1, sizeof rows);
		//memset(cols, 1, sizeof cols);

		rows.clear(); cols.clear();

		For(i,0,m)
		{
			cin >> a >> b;
			a--;
			b--;
			
			rows.insert(a);
			cols.insert(b);

			nr = n - rows.size();
			nc = n - cols.size();

			cout << (long long)nr*nc << " ";
		}
		printf("\n");
	}

	return 0;
}