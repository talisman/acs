import os , subprocess
#-----------
#
acade = "C:/Program Files/Autodesk/AutoCAD 2018/acad.exe"
#-----------
# TODO: add logging , check for fail
#
#
#-----------
def run_a_file (dwgfile,scriptfile):
    args = []
    args.append(acade)
    args.append('/nologo')
    args.append(dwgfile)
    args.append('/b')
    args.append(scriptfile)
    return subprocess.call(args)
#-----------
def main():
    fldr = os.getcwd()
    dwgs = os.listdir(fldr)
    for f in dwgs:
        if f.lower().endswith('.dwg'):
            run_a_file(os.path.join(fldr,f) , 'C:/github/acs/pur.scr')
#-----------




if __name__ == '__main__':
    main()
