// Copyright © 2016 NAME HERE <EMAIL ADDRESS>
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

package cli

import (
	//	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/spf13/cobra"

	"conti-gui/srv"
)

// testCmd represents the test command
var (
	testBaseDir = "/tmp/test-conti-gui"

	testCmd = &cobra.Command{
		Use:   "test",
		Short: "A brief description of your command",
		Long: `A longer description that spans multiple lines and likely contains examples
and usage of using your command. For example:

Cobra is a CLI library for Go that empowers applications.
This application is a tool to generate the needed files
to quickly create a Cobra application.`,

		//	Run: func(cmd *cobra.Command, args []string) {
		//		// TODO: Work your own magic here
		//		fmt.Println("test called")
		//	},

		RunE: func(cmd *cobra.Command, args []string) error {

			//			baseDir := "/tmp"
			htmlFiles_l := []string{"index.html", "wui/index.html"}
			baseDir := filepath.Join(testBaseDir, "test")
			os.MkdirAll(baseDir, 0777)
			tmpDir, err := ioutil.TempDir(baseDir, "system-test-")
			if err != nil {
				return err
			}
			jts := srv.NewJobTypeServer(tmpDir)
			return jts.ServeTestGin(htmlFiles_l)

			//		jts := srv.NewJobTypeServer()
			//		return jts.ServeGin(33333, baseDir, htmlFiles_l)
		},
	}
)

func init() {
	RootCmd.AddCommand(testCmd)

	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// testCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// testCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")

	testCmd.Flags().StringVar(&baseDir, "base-folder", testBaseDir,
		"the directory where all scripts and configuration will be stored and read from")
}
