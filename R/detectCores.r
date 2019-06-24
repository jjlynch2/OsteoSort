#This function is from the parallel package for R and redistributed here under license GPLv2.0

detectCores <- function() {
	if(.Platform$OS.type == "windows") {
		return(as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")))
	} else {
		all.tests = FALSE
		logical = FALSE
		systems <-
			list(darwin = "/usr/sbin/sysctl -n hw.ncpu 2>/dev/null",
				freebsd = "/sbin/sysctl -n hw.ncpu 2>/dev/null",
				linux = "grep processor /proc/cpuinfo 2>/dev/null | wc -l",
				irix  = c("hinv | grep Processors | sed 's: .*::'",
				"hinv | grep '^Processor '| wc -l"),
				solaris = if(logical) "/usr/sbin/psrinfo -v | grep 'Status of.*processor' | wc -l" else "/bin/kstat -p -m cpu_info | grep :core_id | cut -f2 | uniq | wc -l")
		for (i in seq(systems))
			if(all.tests ||
				length(grep(paste0("^", names(systems)[i]), R.version$os)))
				for (cmd in systems[i]) {
					a <- gsub("^ +","", system(cmd, TRUE)[1])
					if (length(grep("^[1-9]", a))) return(as.integer(a))
				}
		NA_integer_
	}
}