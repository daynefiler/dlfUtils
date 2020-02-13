#' @name slurmArray
#' @title Wrapper for rslurm::slurm_apply
#' @description Shorthand wrapper for submitting slurm job arrays using 
#' \code{\link[rslurm]{slurm_apply}}
#' @inheritParams rslurm::slurm_apply
#' @param mem integer of length 1, how much memory to allocate to each job in
#' bytes
#' @param maxJobs integer of length 1, what is the maximum jobs to run at one 
#' time?
#' @param cpu integer of length 1, how many cores for each job? 
#' @details 
#' Submits a slurm array with a job for each row in params. See 
#' \code{\link[rslurm]{slurm_apply}} for more details. 
#' @seealso \code{\link[rslurm]{slurm_apply}}
#' @importFrom rslurm slurm_apply
#' @inherit rslurm::slurm_apply return
#' @export

slurmArray <- function(f, params, jobname = NA, mem = 10000L, 
                       maxJobs = 500L, cpu = 1L, add_objects = NULL,
                       pkgs = rev(.packages()), submit = TRUE,
                       libPaths = NULL) {
  chkInput <- function(x) stopifnot(l1int(type.convert(x)))
  chkInput(mem); chkInput(maxJobs); chkInput(cpu)
  
  nj <- nrow(params)
  a <- sprintf("0-%d%%%d", nj - 1, 500)
  l <- list(mem = mem, 
            array = a, 
            'cpus-per-task' = cpu, 
            error = "%A_%a.err",
            output = "%A_%a.out",
            time = "10-00:00:00")
  rTemplate <- system.file(file.path("slurmTemplates", "silentPkgSlurm.txt"), 
                           package = "dlfUtils")
  slurm_apply(f = f, 
              params = params, 
              nodes = nj, 
              pkgs = pkgs,
              add_objects = add_objects,
              cpus_per_node = cpu,
              jobname = jobname,
              libPaths = libPaths,
              submit = submit,
              slurm_options = l,
              r_template = rTemplate)
}