package srv

import (
	"bufio"
	"bytes"
	"crypto/sha1"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/mgutz/logxi/v1"
	"github.com/rs/xid"
	"github.com/toqueteos/webbrowser"
	"gopkg.in/yaml.v2"

	"conti-gui/wui"
)

const (
	magicLine = "conti-gui job configuration for:"
)

type (
	//	JobTypes struct {
	//		JobTypes []JobType `json:"job_types,omitempty"`
	//	}

	JobType struct {
		//		Id   string `json:"id,omitempty"`
		Id string `json:"id"`
		//		Name string `json:"name,omitempty"`
		Name string `json:"name"`
		//		Jobs []Job `json:"jobs,omitempty"`
		Jobs []Job `json:"jobs"` // `json:"jobs,omitempty"`
	}

	Job struct {
		TypeName     string                 `json:"type_name,omitempty"`
		Id           string                 `json:"job_id,omitempty"`
		Name         string                 `json:"job_name,omitempty"`
		JsonSha1     string                 `json:"json_id,omitempty"`
		YamlSha1     string                 `json:"yaml_id,omitempty"`
		Cmd          string                 `json:"cmd,omitempty"`
		Script       string                 `json:"script,omitempty"`
		ScriptFPath  string                 `json:"script_fpath,omitempty"`
		Debug        map[string]interface{} `json:"debug,omitempty"`
		Nodes        []Wrap                 `json:"root"`
		DefaultNodes []Wrap                 `json:"default_root,omitempty"`
	}

	Record struct {
		//		Id string `json:",omitempty"`
		Id string `json:"id"`
		//		Label       string                 `json:",omitempty"`
		Label string `json:"label"`
		//		Description string                 `json:",omitempty"`
		Description string `json:"descr"`
		//		Value       map[string]interface{} `json:",omitempty"`
		Value map[string]interface{} `json:"value"`
		//		CmdLet      string                 `json:",omitempty"`
		//		CmdLet string `json:"cmdlet"`
		Fmtr map[string]interface{} `json:"fmtr"`
	}

	Wrap struct {
		Rec    Record `json:"rec"`
		Id     int    `json:"id"`
		Parent int    `json:"parent_id"`
	}

	errHandler_T struct {
		err error
	}

	jobSaver_T struct{}
)

func (eh *errHandler_T) safe(steps ...func()) {
	for _, step := range steps {
		if eh.err != nil {
			eh.ifErr(func() { log.Error("ERROR", "err", eh.err) })
			break
		}
		step()
	}
}

func (eh *errHandler_T) ifErr(handle func()) bool {
	if eh.err != nil {
		handle()
	}
	return eh.err != nil
}

func (job *Job) Check(jobTypeExpected string, jobIdExpected string) error {
	//	if jobTypeName != "" && strings.TrimSpace(jobTypeName) != strings.TrimSpace(job.TypeName) {
	jobTypeExpectedNorm := strings.TrimSpace(jobTypeExpected)
	jobTypeNorm := strings.TrimSpace(job.TypeName)
	if jobTypeNorm == "" || (jobTypeExpectedNorm != "" && jobTypeNorm != jobTypeExpectedNorm) {
		msg := fmt.Sprintf("WRONG job Type: '%s': expected '%s'",
			job.TypeName, jobTypeExpected)
		return errors.New(msg)
	}

	jobIdExpectedNorm := strings.TrimSpace(jobIdExpected)
	jobIdNorm := strings.TrimSpace(job.Id)
	if jobIdNorm == "" || (jobIdExpectedNorm != "" && jobIdNorm != jobIdExpectedNorm) {
		//		msg := fmt.Sprintf("MISSING job Id: %#v", *job)
		msg := fmt.Sprintf("WRONG job Id: '%s': expected '%s'",
			job.Id, jobIdExpected)
		return errors.New(msg)
	}
	//	if strings.TrimSpace(job.Name) == "" {
	//		msg := fmt.Sprintf("MISSING job Name: %#v", *job)
	//		return errors.New(msg)
	//	}
	if len(job.Nodes) == 0 {
		msg := fmt.Sprintf("EMPTY job: contains no Nodes: type='%s', id='%s'",
			job.TypeName, job.Id)
		return errors.New(msg)
	}

	recordsById_m := make(map[string]*Wrap)
	for _ /*wrap_i*/, wrap := range job.Nodes {
		_, idKnown := recordsById_m[wrap.Rec.Id]
		if idKnown {
			msg := fmt.Sprintf("CORRUPT job: id '%s' is DUPLICATE",
				wrap.Rec.Id)
			return errors.New(msg)
		}
		recordsById_m[wrap.Rec.Id] = &wrap
	}
	return nil
}

func ServeGin(port int, baseDir string, htmlFiles_l []string) error {
	router := gin.Default()

	router.GET("/", func(c *gin.Context) {
		eh := errHandler_T{}
		var index_b []byte
		eh.safe(func() {
			for _, fn := range htmlFiles_l {
				index_b, eh.err = ioutil.ReadFile(fn)
				if eh.err == nil {
					var n int64
					n, eh.err = io.Copy(c.Writer, bytes.NewBuffer(index_b))
					log.Info("serving local file index.html", "file", fn, "size", n, "err", eh.err)
					break
				}
			}
			if len(index_b) == 0 {
				eh.err = wui.WriteWuiHtml(c.Writer)
				log.Info("serving builtin index.html", "err", eh.err)
			}
		})

		eh.ifErr(func() { c.AbortWithError(http.StatusBadRequest, eh.err) })
	})

	router.GET("/jobs/:jobType", func(c *gin.Context) {
		//		time.Sleep(300 * time.Millisecond)
		eh := errHandler_T{}
		eh.handleJobList(baseDir, c)
	})

	router.GET("/jobs/:jobType/:jobId", func(c *gin.Context) {
		//		time.Sleep(300 * time.Millisecond)
		eh := errHandler_T{}
		eh.handleJobGet(baseDir, c)
	})

	router.POST("/jobs/:jobType", func(c *gin.Context) {
		//		time.Sleep(300 * time.Millisecond)
		eh := errHandler_T{}
		eh.handleJobPost(baseDir, c)
	})

	router.PUT("/jobs/:jobType/:jobId", func(c *gin.Context) {
		//		time.Sleep(300 * time.Millisecond)
		eh := errHandler_T{}
		eh.handleJobPut(baseDir, c)
	})

	router.GET("/ping", func(c *gin.Context) {
		c.JSON(200, gin.H{
			"message": "pong",
		})
	})

	port_s := ""
	if port > 0 && port < 65536 {
		port_s = fmt.Sprintf(":%d", port)
	}
	url := fmt.Sprintf("http://localhost%s/", port_s)

	go func() {
		time.Sleep(100 * time.Millisecond)
		err := webbrowser.Open(url)
		if err != nil {
			log.Error("FAILED to open url in browser", "err", err)
		}
	}()

	return router.Run(port_s) // listen and server on 0.0.0.0:8080
}

func (eh *errHandler_T) handleJobGet(baseDir string, c *gin.Context) error {
	//... parse JSON in post body
	defer c.Request.Body.Close()

	var (
		jobTypeName = c.Param("jobType")
		jobId       = c.Param("jobId")
	)
	log.Info("job get", "job-type", jobTypeName, "job-id", jobId)

	//	jobTypeName := c.Param("jobType")
	if jobTypeName != "RSync" {
		panic("UNKNOWN jobTypeName = " + jobTypeName)
	}

	newJob := Job{
		TypeName: jobTypeName,
		Id:       jobId,
	}
	job, err := newJob.loadYamlScript(eh, baseDir)
	if err != nil || job == nil {
		c.AbortWithError(http.StatusBadRequest, err)
		return err
	}

	log.Trace("returning job", "job", job)

	eh.safe(
		func() { eh.err = job.Check(jobTypeName, jobId) },
		//		func() { newJob.storeYamlScript(eh, baseDir) },
		func() { c.JSON(http.StatusOK, job) },
	)

	eh.ifErr(func() { c.AbortWithError(http.StatusBadRequest, eh.err) })
	return eh.err
}

func (eh *errHandler_T) handleJobList(baseDir string, c *gin.Context) error {
	//... parse JSON in post body
	defer c.Request.Body.Close()

	jobTypeName := c.Param("jobType")
	if jobTypeName != "RSync" {
		panic("UNKNOWN jobTypeName = " + jobTypeName)
	}

	log.Info("loading jobs", "jobTypeName", jobTypeName)

	jobType := JobType{
		Name: jobTypeName,
		Jobs: []Job{},
	}

	eh.safe(func() {
		eh.forAllJobs(
			baseDir, jobTypeName, "*", "",

			// cmdDir, jobTypeName, jobName
			func(oldJobFPath string, oldJob_b []byte) error {
				var cfg_b []byte
				var job Job
				var rootNode *Wrap
				eh.safe(
					func() {
						log.Info("found job", "jobType", jobTypeName,
							"jobfile", oldJobFPath, "size", len(oldJob_b))
						cfg_b, eh.err = extractYamlConfig(oldJob_b)
					},
					func() {
						log.Info("extracted job config", "jobType", jobTypeName,
							"size", len(cfg_b))
						eh.err = yaml.Unmarshal(cfg_b, &job)
					},
					func() {
						job.ScriptFPath = oldJobFPath
						if len(job.Nodes) > 0 {
							rootNode = &job.Nodes[0]
						}
						log.Info("parsed job", "jobType", jobTypeName,
							"job.TypeName", job.TypeName,
							"name", job.Name,
							"nodes", len(job.Nodes),
							"root", rootNode,
						)
						if job.TypeName == jobTypeName {
							jobType.Jobs = append(jobType.Jobs, job)
						}
					},
				)

				eh.ifErr(func() { c.AbortWithError(http.StatusInternalServerError, eh.err) })
				return eh.err
			},
		)
	})

	var buf []byte
	eh.safe(
		func() {
			c.JSON(http.StatusOK, jobType)

			log.Info("List Jobs return", "JobType", jobType)
			buf, eh.err = json.Marshal(jobType)
		},
		func() { log.Info("handleJobList: SUCCESS", "JobType", string(buf)) },
	)

	eh.ifErr(func() { c.AbortWithError(http.StatusBadRequest, eh.err) })
	return eh.err
}

// this call performs 2 tasks:
// 1. if the request body contains a valid job (with an id) the job will be stored on disk [optional]
// 2. (then) it creates a new default job with a new unique id and a unique name and returns it [always]
func (eh *errHandler_T) handleJobPost(baseDir string, c *gin.Context) error {
	var (
		jobTypeName = c.Param("jobType")
		newJobId    = c.Query("newJobId")
	)
	log.Info("job post", "type", jobTypeName, "new-id", newJobId)
	if newJobId != "" {
		return errors.New("POST does not allow to ask for new job id: " + newJobId)
	}

	job, _ /*body_s*/ := readJsonJob(eh, c.Request.Body)

	if job == nil {
		log.Info("posted job EMPTY - not stored")
	} else {
		err := job.Check(jobTypeName, "")
		if err == nil {
			job.storeYamlScript(eh, baseDir)
		} else {
			log.Info("posted job invalid - not stored",
				"id", job.Id, "type", job.TypeName, "name", job.Name,
				"nodes", len(job.Nodes), "err", err.Error())
		}
	}

	newJob := Job{
		TypeName: jobTypeName,
		Id:       "default",
	}
	defaultJob, err := newJob.loadYamlScript(eh, baseDir)

	//	Job struct {
	//		TypeName     string `json:"type_name,omitempty"`
	//		Id           string `json:"job_id,omitempty"`
	//		Name         string `json:"job_name,omitempty"`
	//		JsonSha1     string `json:"json_id,omitempty"`
	//		YamlSha1     string `json:"yaml_id,omitempty"`
	//		Cmd          string `json:"cmd,omitempty"`
	//		Nodes        []Wrap `json:"root"`                   //--`json:"name"`
	//		DefaultNodes []Wrap `json:"default_root,omitempty"` //--`json:"name"`
	//	}

	if err != nil || defaultJob == nil {
		newJob.Nodes = job.DefaultNodes
		log.Info("using hardcoded default job",
			"job.nodes", len(newJob.Nodes),
			"job.defaultNodes", len(newJob.DefaultNodes))
	} else {
		newJob = *defaultJob
		log.Info("using loaded default job",
			"job.nodes", len(newJob.Nodes),
			"job.defaultNodes", len(newJob.DefaultNodes))
	}
	newJob.Id = xid.New().String()
	idLen4 := len(newJob.Id) - 4
	idTail := newJob.Id[idLen4:]

	if newJob.Name == "" {
		newJob.Name = "new"
	}
	newJob.Name += "-" + idTail

	log.Trace("constructed new job", "job", newJob)

	eh.safe(
		func() { eh.err = newJob.Check(jobTypeName, "") },
		func() { newJob.storeYamlScript(eh, baseDir) },
		func() { c.JSON(http.StatusCreated, newJob) },
	)

	eh.ifErr(func() { c.AbortWithError(http.StatusBadRequest, eh.err) })
	return eh.err
}

// save an existing job (with an existing id) to disk and return it or
// another job which is identified by the newJobId query parameter.
func (eh *errHandler_T) handleJobPut(baseDir string, c *gin.Context) error {
	//... parse JSON in post body
	defer c.Request.Body.Close()

	var (
		jobTypeName = c.Param("jobType")
		jobId       = c.Param("jobId")
		newJobId    = c.Query("newJobId")
	)
	log.Info("job put", "type", jobTypeName, "id", jobId, "new-id", newJobId)

	job, body_s := readJsonJob(eh, c.Request.Body)
	eh.safe(func() {
		eh.err = job.Check(jobTypeName, jobId)
		eh.ifErr(func() {
			//			body_s := string(body_b)
			if len(body_s) > 200 {
				body_s = body_s[:200]
			}
			log.Warn("Job.Check failed", "body", body_s)
		})
		//		fmt.Printf("got '%s': '''%s''' from %#v\n", cmd_s, cmdRes, node)
		//			fmt.Printf("got '%s': %#v: %v\n", cmd_s, job, eh.err)
		//		fmt.Printf("got '%s': err=%v\n", cmd_s, eh.err)
	})

	job.storeYamlScript(eh, baseDir)

	//	eh.safe(func() {
	//		c.JSON(http.StatusCreated, job)
	//	})

	eh.safe(func() {
		//		c.JSON(http.StatusCreated, job)

		switch newJobId {
		case "":
			//---- load new default job, create a new id and return it
			// return same job
			fallthrough
		case job.Nodes[0].Rec.Id:
			// return same job
			c.JSON(http.StatusAccepted, job)
		default:
			// try to find job with that id and load and return it

			xJob := *job
			xJob.Id = newJobId
			newJob, err := xJob.loadYamlScript(eh, baseDir)
			if newJob != nil && err == nil {
				c.JSON(http.StatusAccepted, newJob)
			} else {
				c.JSON(http.StatusResetContent, job)
			}
		}
	})

	eh.ifErr(func() { c.AbortWithError(http.StatusBadRequest, eh.err) })
	return eh.err
}

func (eh *errHandler_T) renameToBak(
	cmdDir, cmdName, jobName, oldJobFPath string,
	oldJob_b []byte,
) {
	cs := eh.hashSha1(oldJob_b, nil) //--[:6]
	bakJobFPath := mkJobFPath(cmdDir, cmdName, jobName, "."+cs, true)
	eh.safe(func() { eh.err = os.Rename(oldJobFPath, bakJobFPath) })
}

func (eh *errHandler_T) forAllJobs(
	baseDir, cmdName string,
	jobName string,
	toIgnore string,
	handleFile func(string, []byte) error,
) {
	cmdFName := strings.ToLower(strings.TrimSpace(cmdName))
	cmdDir := filepath.Join(baseDir, cmdFName)
	os.MkdirAll(cmdDir, 0777)

	getJobFPath := func(pat string) string {
		return mkJobFPath(cmdDir, cmdFName, jobName, pat, false)
	}
	jobFPathPat := getJobFPath("*")
	jobFPathIgn := getJobFPath(toIgnore)

	var foundFiles_l []string
	foundFiles_l, eh.err = filepath.Glob(jobFPathPat)
	if len(foundFiles_l) > 0 {
		if len(foundFiles_l) > 1 {
			fmt.Println("found multiple job files")
		}
		for _, oldJobFPath := range foundFiles_l {
			if oldJobFPath == jobFPathIgn {
				continue
			}

			var oldJob_b []byte
			eh.safe(func() { oldJob_b, eh.err = ioutil.ReadFile(oldJobFPath) })
			eh.safe(func() { eh.err = handleFile(oldJobFPath, oldJob_b) })
		}
	}
}

func (eh *errHandler_T) hashSha1(
	obj interface{},
	marshal func(interface{}) ([]byte, error),
) (sha1Hash string) {
	var buf_b []byte
	if marshal == nil {
		buf_b = obj.([]byte)
	} else {
		eh.safe(func() { buf_b, eh.err = marshal(obj) })
	}

	h := sha1.New()
	eh.safe(func() { _, eh.err = h.Write(buf_b) })
	eh.safe(func() { sha1Hash = hex.EncodeToString(h.Sum(nil)) })
	return sha1Hash
}

func extractYamlConfig(job_b []byte) (cfg_b []byte, err error) {
	jobScanner := bufio.NewScanner(bytes.NewBuffer(job_b))
	isYaml := false
	for jobScanner.Scan() {
		line_s := jobScanner.Text()
		if strings.HasPrefix(line_s, "# begin:  "+magicLine) {
			isYaml = true
		} else if strings.HasPrefix(line_s, "# end:  "+magicLine) {
			isYaml = false
		}
		if isYaml {
			//			log.Info("extractYamlConfig", "line", fmt.Sprintf("%#v", []byte(line_s)))
			cfg_b = append(cfg_b, []byte(line_s+"\n")...)
		}
	}
	err = jobScanner.Err()
	return cfg_b, err
}

func mkJobFPath(cmdDir, cmdName, jobName, toIgnore string, subdir bool) string {
	jobFNameX := cmdName + "-" + jobName + toIgnore + ".cgs"
	path_l := []string{cmdDir}
	if subdir {
		path_l = append(path_l, jobName)
	}
	jobFDirX := filepath.Join(path_l...)
	os.MkdirAll(jobFDirX, 0777)

	path_l = append(path_l, jobFNameX)
	jobFPathX := filepath.Join(path_l...)
	return jobFPathX
}

func readJsonJob(eh *errHandler_T, r io.ReadCloser) (*Job, string) {
	var job_b []byte
	defer r.Close()
	eh.safe(func() { job_b, eh.err = ioutil.ReadAll(r) })
	return jobFromScript(eh, job_b, json.Unmarshal, "json-from-wui"), string(job_b)
}

func (job *Job) loadYamlScript(eh *errHandler_T, baseDir string) (*Job, error) {
	var jobCfg_b []byte

	jobFPath := job.getFPath(baseDir, "")
	job_b, err := ioutil.ReadFile(jobFPath)
	if err != nil {
		log.Info("FAILED to load job", "jobFPath", jobFPath, "err", err.Error())
		return nil, err
	}

	eh.safe(
		func() {
			jobCfg_b, eh.err = extractYamlConfig(job_b)
			log.Info("extracted job config",
				//				"job.Type", job.TypeName,
				//				"job.Name", job.Name,
				"size", len(jobCfg_b))
		},
	)
	return jobFromScript(eh, jobCfg_b, yaml.Unmarshal, jobFPath), eh.err
}

func jobFromScript(
	eh *errHandler_T,
	job_b []byte,
	unmarshal func(in []byte, out interface{}) error,
	jobFPath string,
) *Job {
	var newJob Job

	//	log.Trace("jobFromScript", "job_b", string(job_b))

	eh.safe(
		func() {
			eh.err = unmarshal(job_b, &newJob)
			log.Info("Unmarshaled job config",
				"job.Type", newJob.TypeName,
				"job.Name", newJob.Name,
				"size", len(job_b))
		},
		//	)

		//	eh.safe(
		func() {
			newJob.JsonSha1, newJob.YamlSha1 =
				eh.hashSha1(newJob, json.Marshal),
				eh.hashSha1(newJob, yaml.Marshal)
			newJob.ScriptFPath = jobFPath
		},
	)

	//	log.Trace("jobFromScript", "newJob", newJob)

	return &newJob
}

func (job *Job) storeYamlScript(eh *errHandler_T, baseDir string) {
	var job_yb, jobScript_b []byte
	eh.safe(
		func() { job_yb, eh.err = yaml.Marshal(job) },
		func() {
			var (
				lbl string
				val interface{}
			)

			if len(job_yb) < 500 {
				lbl = "job.yaml"
				val = string(job_yb)
			} else {
				lbl = "size"
				val = len(job_yb)
			}
			log.Info("Store job to YAML script",
				"type", job.TypeName,
				"id", job.Id,
				"name", job.Name,
				"nodes", len(job.Nodes),
				"default-nodes", len(job.DefaultNodes),
				lbl, val)
			jobScript_b = job.toScript(job_yb)
			jobFPath := job.getFPath(baseDir, "")
			eh.err = ioutil.WriteFile(jobFPath, jobScript_b, 0777)
		},
	)
}

func (job *Job) getFPath(baseDir string, cs string) string {
	jobTypeNameNorm := strings.TrimSpace(strings.ToLower(job.TypeName))
	cmdDir := filepath.Join(baseDir, jobTypeNameNorm)
	os.MkdirAll(cmdDir, 0777)

	jobNameNorm := strings.TrimSpace(strings.ToLower(job.Name))
	jobIdNorm := strings.TrimSpace(job.Id)
	log.Info("getFPath",
		"job.Type", job.TypeName, "jobTypeNameNorm", jobTypeNameNorm, "cmdDir", cmdDir,
		"jobNameNorm", jobNameNorm,
		"jobIdNorm", jobIdNorm,
		//		"size", len(jobScript_b),
	)

	jobFName := jobTypeNameNorm + "-" + jobIdNorm // jobNameNorm
	if cs != "" {
		jobFName += "." + cs
	}

	jobFName += ".cgs"
	jobFPath := filepath.Join(cmdDir, jobFName)
	return jobFPath
}

//func (job *Job) toYamlScript(eh *errHandler_T) []byte {
//	var job_yb, jobScript_b []byte
//	eh.safe(
//		func() { job_yb, eh.err = yaml.Marshal(job) },
//		func() { jobScript_b = job.toScript(job_yb) },
//	)
//	return jobScript_b
//}

func (job *Job) toScript(job_b []byte) []byte {
	timeStamp := "" // fmt.Sprintf("@ %v", time.Now())
	jobScript_b := []byte(fmt.Sprintf(job.Script,
		//	jobScript_b := []byte(fmt.Sprintf(`#!/bin/bash
		//#
		//# generated script - do not edit
		//#
		//cat <<EOYD | less
		//#
		//# begin:  %[1]s  %[2]s - %[3]s  %[5]s
		//#

		//%[4]s
		//#
		//# end:  %[1]s  %[2]s - %[3]s  %[5]s
		//#
		//EOYD
		//`,
		magicLine, job.TypeName, job.Name, job_b, job.Cmd, timeStamp))
	return jobScript_b
}
