#Maximum Hausdorff
@everywhere function Max_Hausdorff(m1,m2)
	if size(m1,2) == 3
			Res1 = MED3D(m1,m2)
			Res1 = findmax(Res1)[1]
			Res2 = MED3D(m2,m1)
			Res2 = findmax(Res2)[1]
			Res = max(Res1,Res2)
	elseif size(m1,2) == 2
			Res1 = MED2D(m1,m2)
			Res1 = findmax(Res1)[1]
			Res2 = MED2D(m2,m1)
			Res2 = findmax(Res2)[1]
			Res = max(Res1,Res2)
	end
	return Res
end

#Average Hausdorff
@everywhere function Average_Hausdorff(m1,m2)
	if size(m1,2) == 3
			Res1 = MED3D(m1,m2)
			Res1 = mean(Res1)
			Res2 = MED3D(m2,m1)
			Res2 = mean(Res2)
			Res = mean(Res1 + Res2)
	elseif size(m1,2) == 2
			Res1 = MED2D(m1,m2)
			Res1 = mean(Res1)
			Res2 = MED2D(m2,m1)
			Res2 = mean(Res2)
			Res = mean(Res1 + Res2)
	end
	return Res
end

#Dilated Hausdorff
@everywhere function Dilated_Hausdorff(m1,m2)
	if size(m1,2) == 3
			Res1 = MED3D(m1,m2)
			Res2 = MED3D(m2,m1)
			Res = mean((mean(Res1) * std(Res1)) + (mean(Res2) * std(Res2)))
	elseif size(m1,2) == 2
			Res1 = MED2D(m1,m2)
			Res2 = MED2D(m2,m1)
			Res = mean((mean(Res1) * std(Res1)) + (mean(Res2) * std(Res2)))
	end
	return Res
end