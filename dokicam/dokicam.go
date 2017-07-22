package main

import (
	"encoding/xml"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"
)

type DokiCam struct {
	hostname string
	client   *http.Client
}

func NewDokiCam(hostname string) *DokiCam {
	return &DokiCam{
		hostname: hostname,
		client:   &http.Client{},
	}
}

type AFile struct {
	Name string `xml:"NAME"`
	Path string `xml:"FPATH"`
	Size int64  `xml:"SIZE"`
	Time string `xml:"TIME"`
}

type FileList struct {
	XMLName xml.Name `xml:"LIST"`
	Files   []*AFile `xml:"ALLFile>File"`
}

type Status struct {
	Cmd    int `xml:"Cmd"`
	Status int `xml:"Status"`
}

func (dc *DokiCam) getCommon(port int, ep string, custom bool, cmd, par int, str string) (*http.Response, error) {
	// Note: we must prepare the URL manually because order is
	// important ... sigh! :-(
	url := fmt.Sprintf("http://%s:%d/%s", dc.hostname, port, ep)
	sep := "?"
	if custom {
		url += sep + "custom=1"
		sep = "&"
	}
	if cmd > 0 {
		url += sep + fmt.Sprintf("cmd=%d", cmd)
		sep = "&"
	}
	if par >= 0 {
		url += sep + fmt.Sprintf("par=%d", par)
		sep = "&"
	}
	if str != "" {
		url += sep + "str=" + str
		sep = "&"
	}
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return nil, err
	}
	resp, err := dc.client.Do(req)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != 200 {
		resp.Body.Close()
		return nil, fmt.Errorf("Bad Status: %s", resp.Status)
	}
	return resp, nil
}

func (dc *DokiCam) get(port int, ep string, custom bool, cmd, par int, str string, out interface{}) error {
	resp, err := dc.getCommon(port, ep, custom, cmd, par, str)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	body, _ := ioutil.ReadAll(resp.Body)
	if out != nil {
		if err := xml.Unmarshal(body, out); err != nil {
			return err
		}
	}
	return nil
}

func (dc *DokiCam) get2File(port int, ep string, custom bool, cmd, par int, str string, out string) (err error) {
	f := os.Stdout
	if out != "-" {
		f, err = os.Create(out)
		if err != nil {
			return err
		}
		defer f.Close()
	}
	resp, err := dc.getCommon(port, ep, custom, cmd, par, str)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	_, err = io.Copy(f, resp.Body)
	return err
}

func help() {
	fmt.Println("list -> list all files")
	fmt.Println("photo -> photo mode")
	fmt.Println("video -> video mode")
	fmt.Println("ev [-2..2] -> ev compensation")
	fmt.Println("battery -> battery level: 0(full)-4(empty) or charging")
	fmt.Println("shot -> shot a photo")
	fmt.Println("record_on -> start recording")
	fmt.Println("record_off -> stop recording")
	fmt.Println("get [fname, as per list] [dest if specified, can be -] -> get photo/video")
	fmt.Println("rm [fname, as per list] -> deletet photo/video")
	fmt.Println("photo_quality [normal,fine,sfine]")
	fmt.Println("led [on,off]")
	fmt.Println("photo_resolution [24M,20M,12M,8M]")
	fmt.Println("iso [100,200,400,800,1600,auto]")
	fmt.Println("wb [fluorescent,tungsten,cloudy,sunny,auto]")
	fmt.Println("video_resolution [2880x1440,2160x1080,1440x720]")
	fmt.Println("sound [on,off]")
	fmt.Println("video_loop [10,5,3,0]")
	fmt.Println("auto_off [10,5,3,0]")
	fmt.Println("light_hz [60,50]")
	fmt.Println("date [YYYY-MM-DD]")
	fmt.Println("time [HH:MM:SS]")
}

func npar(n int) {
	if len(flag.Args()) < n {
		fmt.Println("Need an argument. Reference:")
		help()
		os.Exit(1)
	}
}

func (dc *DokiCam) simplePar(cmd int, vmap map[string]int) {
	npar(2)
	val := flag.Args()[1]
	v, ok := vmap[val]
	if !ok {
		fmt.Printf("Invalid argument %q. Reference:\n", val)
		help()
		os.Exit(1)
	}
	if err := dc.get(80, "", true, cmd, v, "", nil); err != nil {
		log.Fatalf("Error: %v", err)
	}
}

func main() {
	hostname := flag.String("hostname", "192.168.1.254", "Dokicam hostname/IP address.")
	flag.Parse()
	dc := NewDokiCam(*hostname)
	if len(flag.Args()) == 0 {
		fmt.Println("No command given. Available:")
		help()
		os.Exit(1)
	}
	switch cmd := flag.Args()[0]; cmd {
	case "list":
		var fl FileList
		if err := dc.get(80, "", true, 3015, -1, "", &fl); err != nil {
			log.Fatalf("Error: %v", err)
		}
		for _, f := range fl.Files {
			fname := strings.TrimLeft(f.Path, `A:\DokiCamera\`)
			fmt.Printf("%s\n", strings.Replace(fname, `\`, "/", -1))
		}
	case "photo":
		if err := dc.get(80, "", true, 3001, 0, "", nil); err != nil {
			log.Fatalf("Error: %v", err)
		}
	case "video":
		if err := dc.get(80, "", true, 3001, 1, "", nil); err != nil {
			log.Fatalf("Error: %v", err)
		}
	case "ev":
		npar(2)
		ev, _ := strconv.ParseFloat(flag.Args()[1], 64)
		par := int(6 - 3*ev)
		if err := dc.get(80, "", true, 1010, par, "", nil); err != nil {
			log.Fatalf("Error: %v", err)
		}
	case "battery":
		var r Status
		if err := dc.get(80, "", true, 3019, -1, "", &r); err != nil {
			log.Fatalf("Error: %v", err)
		}
		if r.Status == 5 {
			fmt.Println("Charing")
		} else {
			fmt.Println(r.Status)
		}
	case "shot":
		if err := dc.get(80, "", true, 1001, -1, "", nil); err != nil {
			log.Fatalf("Error: %v", err)
		}
	case "record_on":
		if err := dc.get(80, "", true, 2001, 1, "", nil); err != nil {
			log.Fatalf("Error: %v", err)
		}
	case "record_off":
		if err := dc.get(80, "", true, 2001, 0, "", nil); err != nil {
			log.Fatalf("Error: %v", err)
		}
	case "get":
		npar(2)
		var fname string
		if len(flag.Args()) == 2 {
			parts := strings.Split(flag.Args()[1], "/")
			fname = parts[len(parts)-1]
		} else {
			fname = flag.Args()[2]
		}
		if err := dc.get2File(80, "DokiCamera/"+flag.Args()[1], false, -1, -1, "", fname); err != nil {
			log.Fatalf("Error: %v", err)
		}
	case "rm":
		npar(2)
		fname := `A:\DokiCamera\` + strings.Replace(flag.Args()[1], "/", `\`, -1)
		if err := dc.get(80, "", true, 4003, -1, fname, nil); err != nil {
			log.Fatalf("Error: %v", err)
		}
	case "photo_quality":
		dc.simplePar(1005, map[string]int{"normal": 2, "fine": 1, "sfine": 0})
	case "led":
		dc.simplePar(8025, map[string]int{"on": 1, "off": 0})
	case "photo_resolution":
		dc.simplePar(1002, map[string]int{"24M": 1, "20M": 2, "12M": 3, "8M": 4})
	case "iso":
		dc.simplePar(1009, map[string]int{"100": 1, "200": 2, "400": 3, "800": 4, "1600": 5, "auto": 0})
	case "wb":
		dc.simplePar(1007, map[string]int{"fluorescent": 4, "tungsten": 3, "cloudy": 2, "sunny": 1, "auto": 0})
	case "video_resolution":
		dc.simplePar(2002, map[string]int{"2880x1440": 27, "2160x1080": 13, "1440x720": 26})
	case "sound":
		dc.simplePar(2007, map[string]int{"on": 1, "off": 0})
	case "video_loop":
		dc.simplePar(2003, map[string]int{"10": 3, "5": 2, "3": 1, "0": 0})
	case "auto_off":
		dc.simplePar(3007, map[string]int{"10": 5, "5": 4, "3": 3, "0": 0})
	case "light_hz":
		dc.simplePar(8007, map[string]int{"60": 1, "50": 0})
	case "date":
		npar(2)
		if err := dc.get(80, "", true, 3005, -1, flag.Args()[1], nil); err != nil {
			log.Fatalf("Error: %v", err)
		}
	case "time":
		npar(2)
		if err := dc.get(80, "", true, 3006, -1, flag.Args()[1], nil); err != nil {
			log.Fatalf("Error: %v", err)
		}
	default:
		fmt.Println("Invalid command. Available:")
		help()
		os.Exit(1)
	}
}
