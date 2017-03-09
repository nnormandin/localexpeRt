# p-dimensional isotonic regression. 
#
# xmat is a matrix of predictors; the usual increasing
# order applies, unless decr=TRUE, where the usual decreasing
#  order applies.
#
# the interpolation is given at values in the matrix xpred
#   returned in thetap
#
#   for distinct points only!
#
multiso=function(xmatrix,ys,xpred,decr){
	n=length(ys)
	p=length(xmatrix)/n
	smat=cbind(xmatrix,ys)
	ans=sparsebasis(smat,p)
	amat0=ans$amat
	rank=ans$rank
	m=length(amat0)/2
	y=ans$y
	xmat=ans$xmat
	amat=matrix(0,nrow=m,ncol=n)
	for(i in 1:m){amat[i,amat0[i,1]]=-1;amat[i,amat0[i,2]]=1}
	if(decr){amat=-amat}
	ans=coneproj(y,amat)
	theta=ans$theta
	df=ans$df
# find predicted values at xpred
	if(decr){
		thetap=getpredd(xpred,xmat,theta)
	}else{
		thetap=getpred(xpred,xmat,theta)
	}
	ans=new.env()
	obs=1:n;order=1:n
	for(i in 1:n){order[i]=obs[rank==i]}
	ans$theta=theta[order]
	ans$thetap=thetap
	ans$df=df
	ans
}
	

######################################################
# get predicted values - increasing case
######################################################
getpred=function(xpred,xmat,theta){
	n=length(theta)
	p=length(xmat)/n
	np=length(xpred)/p
	bigger=1:n
	smaller=1:n
	thetap=1:np/0
	for(i in 1:np){
		for(j in 1:n){
			bigger[j]=1;l=0
			while(bigger[j]==1&l<p){
				l=l+1
				if(xpred[i,l]<xmat[j,l]-sm){bigger[j]=0}
			}
			smaller[j]=1;l=0
			while(smaller[j]==1&l<p){
				l=l+1
				if(xpred[i,l]>xmat[j,l]+sm){smaller[j]=0}
			}
		}
		if(sum(bigger)==0&sum(smaller)>0){thetap[i]=min(theta)
			}else if(sum(smaller)==0&sum(bigger)>0){thetap[i]=max(theta)
			}else if(sum(smaller)==0&sum(bigger)==0){thetap[i]=Inf
			}else{
				bval=max(theta[bigger==1])
				sval=min(theta[smaller==1])
				thetap[i]=(bval+sval)/2
			}
	}
	thetap
}
######################################################
# get predicted values - decreasing case
######################################################
getpredd=function(xpred,xmat,theta){
	n=length(theta)
	p=length(xmat)/n
	np=length(xpred)/p
	bigger=1:n
	smaller=1:n
	thetap=1:np/0
	for(i in 1:np){
		for(j in 1:n){
			bigger[j]=1;l=0
			while(bigger[j]==1&l<p){
				l=l+1
				if(xpred[i,l]<xmat[j,l]-sm){bigger[j]=0}
			}
			smaller[j]=1;l=0
			while(smaller[j]==1&l<p){
				l=l+1
				if(xpred[i,l]>xmat[j,l]+sm){smaller[j]=0}
			}
		}
		if(sum(bigger)==0&sum(smaller)>0){thetap[i]=max(theta)
			}else if(sum(smaller)==0&sum(bigger)>0){thetap[i]=min(theta)
			}else{
				bval=min(theta[bigger==1])
				sval=max(theta[smaller==1])
				thetap[i]=(bval+sval)/2
			}
	}
	thetap
}
				
#########################################################
# get the edge vectors for the polar cone 
#    matrix atil has only two columns
#    keeps track of -1 and +1 in rows of amat
#########################################################
sparsebasis=function(prmat,p){
	n=length(prmat)/(p+1)
#sort data -- ascending
	asort=psort(prmat,p)
	xmat=asort$ymat
	xrnk=asort$rank
	sm=1e-10
	anew1=matrix(nrow=n*(n-1)/2,ncol=2)
	comp=matrix(0,nrow=n,ncol=n)
	nr=0
	for(i in 1:(n-1)){
		for(j in (i+1):n){
			bigger=1;l=0
			while(l<p){
				l=l+1
				if(xmat[i,l]>xmat[j,l]+sm){bigger=0}
			}
			if(bigger==1){
				nr=nr+1
				anew1[nr,1]=i;anew1[nr,2]=j
				comp[i,j]=1
			}
		}
	}
	anew2=anew1[1:nr,]
	dump=1:nr<0
	for(i in 1:(n-1)){
		for(j in (i+1):n){
			if(comp[i,j]==1){
				if(j<n){
				for(k in (j+1):n){
					if(comp[j,k]==1&comp[i,k]==1){
						comp[i,k]=2
					}
				}}
			}
		}
	}
	dump=1:nr<0
	id=0
	for(i in 1:(n-1)){
		for(j in (i+1):n){
			if(comp[i,j]>sm){
				id=id+1
				if(comp[i,j]==2){dump[id]=TRUE}
			}
		}
	}
	anew3=anew2[!dump,]
	ans=new.env()
	ans$amat=anew3
	ans$y=xmat[,p+1]
	ans$xmat=xmat[,1:p]
	ans$rank=xrnk
	ans
}



#########################################################
coneproj=function(y,amat){
	n=length(y);m=length(amat)/n
	sm=1e-8;h=1:m<0;obs=1:m;check=0
	delta=-amat;b2=delta%*%y
	if(max(b2)>sm){
		i=min(obs[b2==max(b2)])
		h[i]=TRUE
	}else{check=1;theta=1:n*0}
	while(check==0){
		xmat=matrix(delta[h,],ncol=n)
		a=solve(xmat%*%t(xmat))%*%xmat%*%y
		if(min(a)<(-sm)){
			avec=1:m*0;avec[h]=a
			i=min(obs[avec==min(avec)])
			h[i]=FALSE;check=0
		}else{
			check=1
			theta=t(xmat)%*%a
			b2=delta%*%(y-theta)
			if(max(b2)>sm){
				i=min(obs[b2==max(b2)])		
				h[i]=TRUE;check=0
			}
		}
	}
	bhat=y-theta
	ans=new.env()
	ans$df=n-sum(h)
	ans$theta=bhat
	ans$cf=1:m*0
	ans$cf[h]=a
	ans	
}


######################################################
# sort using partial order
# p+1 is the number of columns of xmat
# but sorts on 1st p columns
#  rank=index of old order
######################################################
psort=function(xmat,p){
	n=length(xmat)/(p+1)
	ymat=xmat*0
	rank=1:n*0
	rows=1:n
	ns=n
	sm=1e-10
	while(ns>1){
		xsm=xmat[1,];ism=1
		for(i in 2:ns){
			smaller=1;l=0
			while(smaller==1&l<p){
				l=l+1
				if(xmat[i,l]>xmat[ism,l]+sm){
					smaller=0
				}
			}
			if(smaller==1){
				bigger=0
				for(l in 1:p){
					if(xmat[ism,l]>xmat[i,l]){bigger=1}
				}
				if(bigger==1){xsm=xmat[i,];ism=i}
			}
		}
		rank[n-ns+1]=rows[ism]
		ymat[n-ns+1,]=xmat[ism,]
		if(ism==1){
			xmat=xmat[2:ns,]
			rows=rows[2:ns]
		}else if(ism==ns){
			xmat=xmat[1:(ns-1),]
			rows=rows[1:(ns-1)]
		}else{
			tmat=matrix(nrow=ns-1,ncol=p+1)
			tmat[1:(ism-1),]=xmat[1:(ism-1),]
			tmat[ism:(ns-1),]=xmat[(ism+1):ns,]
			xmat=tmat
			trow=1:(ns-1)
			trow[1:(ism-1)]=rows[1:(ism-1)]
			trow[ism:(ns-1)]=rows[(ism+1):ns]
			rows=trow
		}	
		ns=ns-1
	}
	ymat[n,]=xmat
	rank[n]=rows
	ans=new.env()
	ans$ymat=ymat
	ans$rank=rank
	ans
}
