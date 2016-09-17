// Copyright © 2016 ContiGuy mrcs.contiguy@mailnull.com
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package srv

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/mgutz/logxi/v1"
	"gopkg.in/yaml.v2"
)

type (
	Job struct {
		TypeName string `json:"type_name,omitempty"`
		Id       string `json:"job_id,omitempty"`
		Name     string `json:"job_name,omitempty"`
		//		JsonSha1       string                 `json:"json_id,omitempty"`
		//		YamlSha1       string                 `json:"yaml_id,omitempty"`
		Cmd            string                 `json:"cmd,omitempty"`
		TypeVersion    Version                `json:"type_version,omitempty"`
		JobVersion     Version                `json:"job_version,omitempty"`
		ScriptFPath    string                 `json:"script_fpath,omitempty"`
		Debug          map[string]interface{} `json:"debug,omitempty"`
		ScriptTemplate string                 `json:"script_template,omitempty"`
		Nodes          []Wrap                 `json:"root"`
		//		DefaultNodes   []Wrap                 `json:"default_root,omitempty"`
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

	Version struct {
		//		Type string `json:"type,omitempty"`
		Hash string `json:"hash,omitempty"`
		Num  int    `json:"num,omitempty"`
	}
)

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

	if len(job.ScriptTemplate) < 10 {
		msg := fmt.Sprintf("CORRUPT job: contains no Script: type='%s', id='%s'",
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

func (job *Job) toScript(job_b []byte) []byte {
	timeStamp := "" // fmt.Sprintf("@ %v", time.Now())
	jobScript_b := []byte(fmt.Sprintf(job.ScriptTemplate,
		magicLine, job.TypeName, job.Name, job_b, job.Cmd, timeStamp))
	return jobScript_b
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
			//			newJob.JsonSha1, newJob.YamlSha1 =
			//				eh.hashSha1(newJob, json.Marshal),
			//				eh.hashSha1(newJob, yaml.Marshal)
			newJob.ScriptFPath = jobFPath
		},
	)

	//	log.Trace("jobFromScript", "newJob", newJob)

	return &newJob
}

func (job *Job) storeYamlScript(eh *errHandler_T, baseDir string) {
	var job_yb, jobScript_b []byte
	var jobVersionHash, jobTypeVersionHash string
	eh.safe(
		func() {
			jobVersionHash = eh.hashSha1(job.extractVersion())
		},
		func() {
			if jobVersionHash != job.JobVersion.Hash {
				job.JobVersion.Num++
				job.JobVersion.Hash = jobVersionHash
			}
		},
		func() {
			jobTypeVersionHash = eh.hashSha1(job.extractTypeVersion())
		},
		func() {
			if jobTypeVersionHash != job.TypeVersion.Hash {
				job.TypeVersion.Num++
				job.TypeVersion.Hash = jobTypeVersionHash
			}
		},

		func() { job_yb, eh.err = yaml.Marshal(job) },
		func() {
			//			var (
			//				lbl string
			//				val interface{}
			//			)

			//			if len(job_yb) < 500 {
			//				lbl = "job.yaml"
			//				val = string(job_yb)
			//			} else {
			//				lbl = "size"
			//				val = len(job_yb)
			//			}

			//			lbl := "job.yaml"
			//			val := string(job_yb)
			//			if len(job_yb) > 500 {
			//				lbl = "job.yaml[:500]"
			//				val = string(job_yb[:500]) + " ..."
			//			}

			//			jobFPath := job.getFPath(baseDir, "")
			//			job.ScriptFPath = jobFPath
			job.ScriptFPath = job.getFPath(baseDir, "")
			jobScript_b = job.toScript(job_yb)

			job.log( /*job_yb*/ )

			eh.err = ioutil.WriteFile(job.ScriptFPath, jobScript_b, 0777)

			//			log.Trace("Store job to YAML script",
			//				//			log.Info("Store job to YAML script",
			//				"jobFPath", jobFPath,
			//				"type", job.TypeName,
			//				"id", job.Id,
			//				"name", job.Name,
			//				"nodes", len(job.Nodes),
			//				"default-nodes", len(job.DefaultNodes),
			//				"err", eh.err.Error(),
			//				lbl, val)
		},
	)
}

func (job *Job) extractVersion( //--eh *errHandler_T,
) Job {
	//	Job struct {
	//		TypeName string `json:"type_name,omitempty"`
	//		Id       string `json:"job_id,omitempty"`
	//		Name     string `json:"job_name,omitempty"`
	//		//		JsonSha1       string                 `json:"json_id,omitempty"`
	//		//		YamlSha1       string                 `json:"yaml_id,omitempty"`
	//		Cmd            string                 `json:"cmd,omitempty"`
	//		TypeVersion    Version                `json:"type_version,omitempty"`
	//		JobVersion     Version                `json:"job_version,omitempty"`
	//		ScriptFPath    string                 `json:"script_fpath,omitempty"`
	//		Debug          map[string]interface{} `json:"debug,omitempty"`
	//		ScriptTemplate string                 `json:"script_template,omitempty"`
	//		Nodes          []Wrap                 `json:"root"`
	//		//		DefaultNodes   []Wrap                 `json:"default_root,omitempty"`
	//	}

	vJob := *job
	vJob.TypeVersion = Version{}
	vJob.JobVersion = Version{}
	vJob.ScriptFPath = ""
	vJob.Debug = nil
	return vJob
}

func (job *Job) extractTypeVersion( //--eh *errHandler_T,
) Job {
	//	Job struct {
	//		TypeName string `json:"type_name,omitempty"`
	//		Id       string `json:"job_id,omitempty"`
	//		Name     string `json:"job_name,omitempty"`
	//		//		JsonSha1       string                 `json:"json_id,omitempty"`
	//		//		YamlSha1       string                 `json:"yaml_id,omitempty"`
	//		Cmd            string                 `json:"cmd,omitempty"`
	//		TypeVersion    Version                `json:"type_version,omitempty"`
	//		JobVersion     Version                `json:"job_version,omitempty"`
	//		ScriptFPath    string                 `json:"script_fpath,omitempty"`
	//		Debug          map[string]interface{} `json:"debug,omitempty"`
	//		ScriptTemplate string                 `json:"script_template,omitempty"`
	//		Nodes          []Wrap                 `json:"root"`
	//		//		DefaultNodes   []Wrap                 `json:"default_root,omitempty"`
	//	}
	//	Record struct {
	//		//		Id string `json:",omitempty"`
	//		Id string `json:"id"`
	//		//		Label       string                 `json:",omitempty"`
	//		Label string `json:"label"`
	//		//		Description string                 `json:",omitempty"`
	//		Description string `json:"descr"`
	//		//		Value       map[string]interface{} `json:",omitempty"`
	//		Value map[string]interface{} `json:"value"`
	//		//		CmdLet      string                 `json:",omitempty"`
	//		//		CmdLet string `json:"cmdlet"`
	//		Fmtr map[string]interface{} `json:"fmtr"`
	//	}

	//	Wrap struct {
	//		Rec    Record `json:"rec"`
	//		Id     int    `json:"id"`
	//		Parent int    `json:"parent_id"`
	//	}

	eh := errHandler_T{}
	var vJob, tvJob Job
	var hJob1, hJob2, hJob3, hJobVers1, hJobVers2, hJobTypeVers string
	hJob1 = eh.hashSha1(job)
	eh.safe(
		func() {
			vJob = job.extractVersion()
			hJobVers1 = eh.hashSha1(vJob)
		},
		func() { hJob2 = eh.hashSha1(job) },
		func() {
			tvJob = vJob
			tvJob.Nodes = make([]Wrap, len(job.Nodes))
			for i, w := range job.Nodes {
				w.Rec.Value = nil
				tvJob.Nodes[i] = w
			}
			hJobTypeVers = eh.hashSha1(tvJob)
		},
		func() { hJob3 = eh.hashSha1(job) },
		func() { hJobVers2 = eh.hashSha1(vJob) },
		func() {
			//			errStr := ""
			//			if eh.err != nil {
			//				errStr = eh.err.Error()
			//			}
			log.Info("job version hashes",
				"job 1", hJob1,
				"job 2", hJob2,
				"job 3", hJob3,
				"job 1 == 2", (hJob1 == hJob2),
				"job 2 == 3", (hJob2 == hJob3),
				"job version 1", hJobVers1,
				"job version 2", hJobVers2,
				"job version 1 == 2", (hJobVers1 == hJobVers2),
				"job type version", hJobTypeVers,
				//				"err", errStr, //--eh.err.Error(),
			)
		},
	)
	return tvJob
}

func (job *Job) log( //--eh *errHandler_T,
//	job_yb []byte,

// baseDir string
) {
	//			var (
	//				lbl string
	//				val interface{}
	//			)

	//			if len(job_yb) < 500 {
	//				lbl = "job.yaml"
	//				val = string(job_yb)
	//			} else {
	//				lbl = "size"
	//				val = len(job_yb)
	//			}

	job_yb, err := yaml.Marshal(job)

	lbl := "job.yaml"
	val := string(job_yb)
	if len(job_yb) > 500 {
		lbl = "job.yaml[:500]"
		val = string(job_yb[:500]) + " ..."
	}

	//	jobFPath := job.getFPath(baseDir, "")
	//	job.ScriptFPath = jobFPath
	//	jobScript_b = job.toScript(job_yb)
	//	eh.err = ioutil.WriteFile(jobFPath, jobScript_b, 0777)
	log.Trace("Store job to YAML script",
		//			log.Info("Store job to YAML script",
		"jobFPath", job.ScriptFPath,
		"type", job.TypeName,
		"id", job.Id,
		"name", job.Name,
		"nodes", len(job.Nodes),
		//		"default-nodes", len(job.DefaultNodes),
		"err", err, //-.Error(),
		lbl, val)
}
