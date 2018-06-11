import os , subprocess
#
#
acade = "C:/Program Files/Autodesk/AutoCAD 2018/acad.exe"
# TODO: add logging , check for fail 
#
def run_a_file (dwgfile,scriptfile):
    args = []
    args.append(acade)
    args.append('/nologo')
    args.append(dwgfile)
    args.append('/b')
    args.append(scriptfile)
    return subprocess.call(args)

#
def main():
    run_a_file('C:/7777/ProjectsWork_Revit2017/1467/M145/FloorBlocks/as32.dwg' , 'C:/github/acs/pur.scr')
#
if __name__ == '__main__':
    main()
