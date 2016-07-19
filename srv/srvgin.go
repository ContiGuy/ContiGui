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
	"github.com/toqueteos/webbrowser"
	"gopkg.in/yaml.v2"

	"conti-gui/wui"
)

type (
	JobTypes struct {
		JobTypes []JobType `json:"job_types,omitempty"`
	}

	JobType struct {
		//		Id   string `json:"id,omitempty"`
		Id string `json:"id"`
		//		Name string `json:"name,omitempty"`
		Name string `json:"name"`
		//		Jobs []Job `json:"jobs,omitempty"`
		Jobs []Job `json:"jobs"` // `json:"jobs,omitempty"`
	}

	Job struct {
		Name     string `json:"name,omitempty"`
		TypeName string `json:"type_name,omitempty"`
		//		Id   string
		JsonSha1 string `json:"json_id,omitempty"`
		YamlSha1 string `json:"yaml_id,omitempty"`

		//		Root     Node   `json:",omitempty"` //--`json:"name"`

		//--Root Node `json:"root"` //--`json:"name"`
		Nodes []Wrap `json:"root"` //--`json:"name"`
	}

	//type alias Record =
	//  { id       : Id
	//  , label    : String
	//  , descr    : String
	//  , value    : Value
	//  , fmtr     : Formatter
	//  }

	Record struct {
		//		Id string `json:",omitempty"`
		Id string `json:"id"`
		//		Type        string `json:",omitempty"`
		//		Label       string                 `json:",omitempty"`
		Label string `json:"label"`
		//		Description string                 `json:",omitempty"`
		Description string `json:"descr"`
		//		Value       map[string]interface{} `json:",omitempty"`
		Value map[string]interface{} `json:"value"`
		//		CmdLet      string                 `json:",omitempty"`
		//		CmdLet string `json:"cmdlet"`
		//		//		Kids        []*Node                `json:",omitempty"`
		//		Kids []*Node `json:"kids"`
		//		//		IsActive *bool `json:"active"`
		Fmtr map[string]interface{} `json:"fmtr"`
	}

	//type alias Wrap =
	//  { rec     : Record
	//  , id      : Int
	//  , parent  : Int
	//  }
	Wrap struct {
		Rec    Record `json:"rec"`
		Id     int    `json:"id"`
		Parent int    `json:"par-id"`
	}

	//type alias Node =
	//  { rec  : Record
	//  , kids : Tree
	//  }

	//type Tree = Kids (List Node)

	//type Value
	//  = BoolValue Bool
	//  | StringValue String
	//  | RootCmd
	//  | Group Orientation
	//  | Switch Id

	//type Orientation
	//  = Vertical
	//  | Horizontal
	//  | Disoriented

	//type Formatter
	//  = BoolFmtr String String
	//  | StringFmtr String
	//  | KidsListFmtr String String
	//--  | KidsByIdFmtr String String
	//  | SelectedKidFmtr

	//	Node struct {
	//		//		Id string `json:",omitempty"`
	//		Id string `json:"id"`
	//		//		Type        string `json:",omitempty"`
	//		//		Label       string                 `json:",omitempty"`
	//		Label string `json:"label"`
	//		//		Description string                 `json:",omitempty"`
	//		Description string `json:"descr"`
	//		//		Value       map[string]interface{} `json:",omitempty"`
	//		Value map[string]interface{} `json:"value"`
	//		//		CmdLet      string                 `json:",omitempty"`
	//		CmdLet string `json:"cmdlet"`
	//		//		Kids        []*Node                `json:",omitempty"`
	//		Kids []*Node `json:"kids"`
	//		//		IsActive *bool `json:"active"`
	//	}

	//	Node struct {
	//		//		Id string `json:",omitempty"`
	//		Id string `json:"id"`
	//		//		Type        string `json:",omitempty"`
	//		//		Label       string                 `json:",omitempty"`
	//		Label string `json:"label"`
	//		//		Description string                 `json:",omitempty"`
	//		Description string `json:"descr"`
	//		//		Value       map[string]interface{} `json:",omitempty"`
	//		Value map[string]interface{} `json:"value"`
	//		//		CmdLet      string                 `json:",omitempty"`
	//		CmdLet string `json:"cmdlet"`
	//		//		Kids        []*Node                `json:",omitempty"`
	//		Kids []*Node `json:"kids"`
	//		//		IsActive *bool `json:"active"`
	//	}

	//	nodesById_M map[string]*Node

	//	cmdletsById_M map[string]string

	errHandler_T struct {
		err error
	}
)

func (eh *errHandler_T) safe(step func()) {
	if eh.err == nil {
		step()
		//		eh.ifErr(func() { log.Printf("ERROR: %s\n", eh.err) })
		eh.ifErr(func() { log.Error("ERROR", "err", eh.err) })
	}
}

func (eh *errHandler_T) ifErr(handle func()) bool {
	if eh.err != nil {
		handle()
	}
	return eh.err != nil
}

func (job *Job) Check() error {
	if strings.TrimSpace(job.Name) == "" {
		msg := fmt.Sprintf("MISSING job Name: %#v", *job)
		return errors.New(msg)
	}
	//	if strings.TrimSpace(job.Root.Label) == "" {
	//		//		return errors.New("MISSING job Root Label")
	//		msg := fmt.Sprintf("MISSING job Root Label: %#v", *job)
	//		return errors.New(msg)
	//	}
	//	return job.Root.ProcessTree()

	return nil
}

//func (rsJob *RSyncJob) Check() error {
//	//	if strings.TrimSpace(job.Name) == "" {
//	//		msg := fmt.Sprintf("MISSING job Name: %#v", *job)
//	//		return errors.New(msg)
//	//	}
//	err := rsJob.Job.Check()
//	if err != nil {
//		return err
//	}

//	if strings.TrimSpace(rsJob.Root.Label) == "" {
//		//		return errors.New("MISSING job Root Label")
//		msg := fmt.Sprintf("MISSING job Root Label: %#v", *rsJob)
//		return errors.New(msg)
//	}
//	return rsJob.Root.ProcessTree()
//}

//func (node *Node) ProcessTree() error {
//	nodesById_m := make(nodesById_M)

//	cnf := func(n *Node) error {
//		return n.CheckNode(nodesById_m)
//	}

//	return node.WalkTree(cnf)
//}

//func (node *Node) CheckNode(nodesById_m nodesById_M) error {
//	altNode, ok := nodesById_m[node.Id]
//	if ok {
//		errMsg := fmt.Sprintf("Duplicate ID '%s':  %+v  <->  %+v",
//			node.Id, altNode, node)
//		return errors.New(errMsg)
//	}
//	nodesById_m[node.Id] = node
//	return nil
//}

//func (node *Node) WalkTree(cnf func(*Node) error) error {
//	for _, kid := range node.Kids {
//		err := kid.WalkTree(cnf)
//		if err != nil {
//			return err
//		}
//	}

//	return cnf(node)
//}

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

	router.POST("/jobs/:jobType", func(c *gin.Context) {
		time.Sleep(300 * time.Millisecond)
		eh := errHandler_T{}
		eh.handleJobPost(baseDir, c)
	})

	router.GET("/jobs/:jobType", func(c *gin.Context) {
		time.Sleep(300 * time.Millisecond)
		eh := errHandler_T{}
		eh.handleJobList(baseDir, c)
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
			//			log.Printf("FAILED to open url in browser: %s\n", err)
			log.Error("FAILED to open url in browser", "err", err)
		}
	}()

	return router.Run(port_s) // listen and server on 0.0.0.0:8080
}

func (eh *errHandler_T) handleJobList(baseDir string, c *gin.Context) error {
	//... parse JSON in post body
	defer c.Request.Body.Close()

	jobTypeName := c.Param("jobType")
	if jobTypeName != "RSync" {
		panic("UNKNOWN jobTypeName = " + jobTypeName)
	}

	log.Info("loading jobs", "jobTypeName", jobTypeName)

	//	var jobs_ml []gin.H
	//	var jobs_l []Job

	//	var jobTypes []JobType
	//	jobTypes_m := make(map[string]JobType)
	jobType := JobType{
		Name: jobTypeName,
		Jobs: []Job{},
	}

	eh.safe(func() {
		eh.forAllJobs(
			baseDir, jobTypeName, "*", //-- jobName,
			"", //	"."+cs, // toIgnore,

			// cmdDir, jobTypeName, jobName
			func(oldJobFPath string, oldJob_b []byte) error {
				log.Info("found job", "jobType", jobTypeName,
					"jobfile", oldJobFPath, "size", len(oldJob_b))

				var cfg_b []byte
				eh.safe(func() {
					cfg_b, eh.err = extractYamlConfig(oldJob_b)
				})
				log.Info("extracted job config", "jobType", jobTypeName,
					//--"jobfile", oldJobFPath,
					"size", len(cfg_b))

				var job Job
				//				var rsJob RSyncJob
				eh.safe(func() {
					eh.err = yaml.Unmarshal(cfg_b, &job) //-- &rsJob)
				})
				log.Info("parsed job", "jobType", jobTypeName,
					//					"jobfile", oldJobFPath,
					//					 "job.TypeName", rsJob.Job.TypeName, "name", rsJob.Job.Name)
					"job.TypeName", job.TypeName,
					"name", job.Name,
					"nodes", len(job.Nodes),
					"root", job.Nodes[0],
				)

				//	JobTypes struct {
				//		JobTypes []JobType `json:"job_types"`
				//	}

				//	JobType struct {
				//		Id   string `json:"id"`
				//		Name string `json:"name"`
				//		Jobs []Job  `json:"jobs"`
				//	}

				//	RSyncJob struct {
				//		Job  Job
				//		Root Node //--`json:"name"`
				//	}

				//	Job struct {
				//		Name     string `json:"name"`
				//		TypeName string `json:"type_name"`
				//		//		Id   string
				//		JsonSha1 string `json:"json_id"`
				//		YamlSha1 string `json:"yaml_id"`
				//		//--Root     Node   //--`json:"name"`
				//	}

				eh.safe(func() {
					//					if rsJob.Job.TypeName == jobTypeName {
					//						jobType.Jobs = append(jobType.Jobs, rsJob.Job)
					if job.TypeName == jobTypeName {
						// FIXME: remove! this is just here to minimize the data traffic
						//						job.Root.Kids = []*Node{}
						//						job.Root = Node{}
						jobType.Jobs = append(jobType.Jobs, job)
					}
				})

				//				job_m := gin.H{
				//					"name":    job.Name,
				//					"json_id": job.JsonSha1,
				//					"yaml_id": job.YamlSha1,
				//					"cmd":     job.Root.CmdLet,
				//				}

				//				jobs_ml = append(jobs_ml, job_m)
				//				jobs_l = append(jobs_l, job)

				eh.ifErr(func() { c.AbortWithError(http.StatusInternalServerError, eh.err) })
				return eh.err
			},
		)
	})

	var buf []byte
	eh.safe(func() {
		jt := JobTypes{
			JobTypes: []JobType{jobType},
		}
		c.JSON(http.StatusOK, jt)

		log.Info("List Jobs return", "JobTypes", jt)

		//		buf, eh.err = json.MarshalIndent(jt, "", "  ")
		buf, eh.err = json.Marshal(jt)

		//		res := gin.H{
		//			"job_types": JobTypes
		//				"name": jobTypeName,
		//				"id":   "x0",
		//				"jobs": jobs_l,
		//			},
		////			"job_type": gin.H{
		////				"name": jobTypeName,
		////				"id":   "x0",
		////				"jobs": jobs_l,
		////			},
		//		}
		//		c.JSON(http.StatusOK, res)
	})

	eh.safe(func() {
		fmt.Printf("returned JobTypes =\n%s\n", buf)
	})
	eh.ifErr(func() { c.AbortWithError(http.StatusBadRequest, eh.err) })
	return eh.err
}

func /*(eh *errHandler_T)*/ extractYamlConfig(job_b []byte) (cfg_b []byte, err error) {
	//	var cfg_b []byte
	jobScanner := bufio.NewScanner(bytes.NewBuffer(job_b))
	isYaml := false
	for jobScanner.Scan() {
		line_s := jobScanner.Text()
		if strings.HasPrefix(line_s, "# begin:  CoLiGui job configuration for:") {
			isYaml = true
		} else if strings.HasPrefix(line_s, "# end:  CoLiGui job configuration for:") {
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

func (eh *errHandler_T) handleJobPost(baseDir string, c *gin.Context) error {
	jobTypeName := c.Param("jobType")
	//		id_s := c.Param("id")

	//... parse JSON in post body
	defer c.Request.Body.Close()

	var body_b []byte
	eh.safe(func() { body_b, eh.err = ioutil.ReadAll(c.Request.Body) })
	//	msg1 := fmt.Sprintf("POSTed to /job/%s: %d bytes ...", cmd_s, len(body_b))
	//	fmt.Println(msg1)
	log.Info("POSTed to /job", "jobType", jobTypeName, "bytes", len(body_b))

	var job Job
	//	var rsJob RSyncJob
	eh.safe(func() { eh.err = json.Unmarshal(body_b, &job) }) //--&rsJob) })
	eh.safe(func() {
		//		eh.err = rsJob.Check()
		eh.err = job.Check()
		eh.ifErr(func() {
			body_s := string(body_b)
			if len(body_s) > 200 {
				body_s = body_s[:200]
			}
			log.Warn("Job.Check failed", "body", body_s)
		})
		//		fmt.Printf("got '%s': '''%s''' from %#v\n", cmd_s, cmdRes, node)
		//			fmt.Printf("got '%s': %#v: %v\n", cmd_s, job, eh.err)
		//		fmt.Printf("got '%s': err=%v\n", cmd_s, eh.err)
	})

	//	job := &rsJob.Job
	eh.safe(func() {
		//		jsonSha1 := eh.hashSha1(job, json.Marshal)
		//		job.YamlSha1 = eh.hashSha1(job, yaml.Marshal)
		//		job.JsonSha1 = jsonSha1
		job.JsonSha1, job.YamlSha1 =
			eh.hashSha1(job, json.Marshal),
			eh.hashSha1(job, yaml.Marshal)
	})

	var job2_yb []byte
	eh.safe(func() {
		job2_yb, eh.err = yaml.Marshal(job) //--rsJob)
	})
	log.Info("Marshal job to YAML", "jobType", jobTypeName, "size", len(job2_yb))
	//	msg2 := fmt.Sprintf("MarshalIndent /job/%s: %d bytes ...",
	//		cmd_s, len(job2_yb))
	//	fmt.Println(msg2)
	//		job.Root.CmdLet += msg2

	timeStamp := "" // fmt.Sprintf("@ %[4]v", time.Now())
	jobScript_b := []byte(fmt.Sprintf(`#!/bin/bash
#
# generated script - do not edit
#
cat <<EOYD | less
#
# begin:  CoLiGui job configuration for:  %[1]s - %[2]s  %[4]s
#

%[3]s
#
# end:  CoLiGui job configuration for:  %[1]s - %[2]s  %[4]s
#
EOYD
`,
		//		//		rsJob.Root.Label, rsJob.Job.Name, job2_yb, timeStamp))
		//		job.Root.Label, job.Name, job2_yb, timeStamp))
		job.TypeName, job.Name, job2_yb, timeStamp))

	//	//	cmdFName := strings.TrimSpace(strings.ToLower(rsJob.Root.Label))
	//	cmdFName := strings.TrimSpace(strings.ToLower(job.Root.Label))
	cmdFName := strings.TrimSpace(strings.ToLower(job.TypeName))
	cmdDir := filepath.Join(baseDir, cmdFName)
	os.MkdirAll(cmdDir, 0777)

	jobName := strings.TrimSpace(strings.ToLower(job.Name))
	log.Info("generated job script",
		"jobType", jobTypeName, "cmdFName", cmdFName, "cmdDir", cmdDir,
		"jobName", jobName, "size", len(jobScript_b))

	var jobFPath, cs string
	eh.safe(func() {
		cs = eh.hashSha1(jobScript_b, nil)[:6]
		jobFName := cmdFName + "-" + jobName + "." + cs + ".cgs"
		jobFPath = filepath.Join(cmdDir, jobFName)
	})

	haveToSaveJob := true

	fInfo, err := os.Stat(jobFPath)
	if err == nil && !fInfo.IsDir() {
		var oldJob_b []byte
		eh.safe(func() { oldJob_b, eh.err = ioutil.ReadFile(jobFPath) })

		haveToSaveJob = bytes.Compare(jobScript_b, oldJob_b) != 0
	}

	cmdMsg := "# job already known, not saved: " + jobFPath //-job.Root.CmdLet
	if haveToSaveJob {
		eh.safe(func() { eh.err = ioutil.WriteFile(jobFPath, jobScript_b, 0777) })
		cmdMsg = "# job saved as: " + jobFPath
	}

	//	eh.safe(func() {
	eh.safe(func() {
		eh.forAllJobs(
			baseDir, cmdFName, jobName, "."+cs, // toIgnore,

			// cmdDir, cmdName, jobName
			func(oldJobFPath string, oldJob_b []byte) error {
				eh.renameToBak(cmdDir, cmdFName, jobName, oldJobFPath, oldJob_b)
				return eh.err
			},
		)
	})
	//	})

	eh.safe(func() {
		res := gin.H{
			"job_name": job.Name,
			"json_id":  job.JsonSha1,
			"yaml_id":  job.YamlSha1,
			"cmd":      cmdMsg, // job.Root.CmdLet,
		}
		c.JSON(http.StatusCreated, res)
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
		//		return mkJobFPath(cmdDir, cmdName, "*", pat, false)
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
	//	size int,
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
