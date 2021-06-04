int main()
{
	//initializeaza dataset
	int numChanged=0, examineAll=1;
	while(numChanged > 0 || examineAll==1)
	{
		numChanged=0;
		if(examineAll==1) //parcurge toate exemplele
		{
			for(int i=0; i<N;i++)
			{
				numChanged+=examineExample(i);
			}
		}
		else //parcurge doar exemplele unde alpha-urile is nemarginite
		{
			for(int i=0; i<N;i++)
			{
				if(alphas[i]!=0 && alphas[i] !=C)
					numChanged+=examineExample(i);
			}
		}
		if(examineAll == 1)
			examineAll = 0;
		else if(numChanged == 0)
			examineAll=1;
	}
}

int examineExample(int i1)
{
	y1 = target[i1];
	alpha1 = alphas[i1];
	E1=w*x1+b-y1
	
	//verifica KKT
	if((y1*E1 < -tol && alpha1 < C) || (y1*E1 > tol && alpha1 >0))
	{
		//incercare cu a doua euristica
		for(i2:trainSet)
		{
			if(alphas[i2] > 0 && alphas[i2]<C)
			{
				if(takeStep(i1,i2)
					return 1;
			}
		}
		for(i2:trainSet)
		{
			if(takeStep(i1,i2)
				return 1;
		}			
	}
	return 0;	
}

int takeStep(int i1, int i2)
{
	if(i1 == i2)
		return 0;
	
	alpha1 = alphas[i1];
	y1=target[i1];
	E1=w*x1+b-y1
	
	alpha2 = alphas[i2];
	y2=target[i2];
	E2=w*x2+b-y2
	
	if(y1==y2)
	{
		L=max(0,alpha1 + alpha2-C);
		H=min(C,C+alpha1+alpha2);
	}
	else
	{
		L=max(0,alpha2 - alpha1);
		H=min(C,C+alpha2-alpha1);
	}
	
	if(L==H)
		return 0;
	
	if(eta > 0){
		newa2=alpha2+y2*(E2-E1)/eta;
		if(newa2<L)
			newa2=L;
		else if(newa2<H)
			newa2=H;
	}
	else if (y1(E1-E2) <0)
		newa2=H
	else
		newa2=L
	
	if(abs(newa2-alpha2) < eps*(newa2+alpha2+eps))
		return 0;
	
	newa1=alpha1-s*(newa2-alpha2);
	modifica b;
	modifica w;
	modifica alphas;
}

