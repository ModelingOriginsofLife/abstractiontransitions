function rval = evalgaus(data, mu)
  errdata = exp(-(data-mu).^2 / 0.02^2);
  
  rval = sum(errdata);
endfunction

function rval = gausfit(data)
  best = 0;
  bmu = 0;
  mu = 0;
  
  for mu = 0:0.001:1
    val = evalgaus(data, mu);
    
    if (val>best) 
      best = val;
      bmu = mu;
    end
  end
  
  rval = bmu;      
endfunction

arglist = argv();
A = load(arglist{1});

B(:,1)=A(:,1);
B(:,2)=1-A(:,2)./A(:,3);

y = unique(B(:,1));

C(:,1) = y;

l=size(y)(1);

for i=1:l
  subdata = B(find(B(:,1) == y(i)),2);
  C(i,2) = mean(subdata);
  C(i,3) = std(subdata);
  C(i,4) = gausfit(subdata);  
end

save("-ascii", arglist{2}, "C");