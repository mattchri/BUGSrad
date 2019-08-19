import numpy as np
def read_profile(profFile):
    f = open(profFile, 'r')
    lines = f.readlines()
    nlev = len(lines)
    varN = ['z','p','t','q','ql','qi','qr']
    varU = ['km','hPa','K','kg/kg','kg/kg','kg/kg','kg/kg']
    z = []
    p = []
    t = []
    q = []
    ql = []
    qi = []
    qr = []
    for i in range(nlev):
        z.append(float((lines[i].split(','))[0]))
        p.append(float((lines[i].split(','))[1]))
        t.append(float((lines[i].split(','))[2]))
        q.append(float((lines[i].split(','))[3]))
        ql.append(float((lines[i].split(','))[4]))
        qi.append(float((lines[i].split(','))[5]))
        qr.append(float((lines[i].split(','))[6]))
    z = np.asarray(z)
    p = np.asarray(p)
    t = np.asarray(t)
    q = np.asarray(q)
    ql = np.asarray(ql)
    qi = np.asarray(qi)
    qr = np.asarray(qr)
    f.close()
    output = {"z": z, "p":p, "t":t, "q":q, "ql":ql, "qi":qi, "qr":qr}
    return output

#Read BUGSrad File
def read_bugsrad(BUGSrad_File):
    f = open(BUGSrad_File, 'r')
    lines = f.readlines()
    nlev = len(lines)
    swdn = []
    swup = []
    lwdn = []
    lwup = []
    swdn_clr = []
    swup_clr = []
    lwdn_clr = []
    lwup_clr = []
    for i in range(2,nlev):
        swdn.append(float((lines[i].split(','))[1]))
        swup.append(float((lines[i].split(','))[2]))
        lwdn.append(float((lines[i].split(','))[3]))
        lwup.append(float((lines[i].split(','))[4]))
        swdn_clr.append(float((lines[i].split(','))[5]))
        swup_clr.append(float((lines[i].split(','))[6]))
        lwdn_clr.append(float((lines[i].split(','))[7]))
        lwup_clr.append(float((lines[i].split(','))[8]))
    swdn = np.asarray(swdn)
    swup = np.asarray(swup)
    lwdn = np.asarray(lwdn)
    lwup = np.asarray(lwup)
    swdn_clr = np.asarray(swdn_clr)
    swup_clr = np.asarray(swup_clr)
    lwdn_clr = np.asarray(lwdn_clr)
    lwup_clr = np.asarray(lwup_clr)
    f.close()
    output = {"swdn":swdn,"swup":swup,"lwdn":lwdn,"lwup":lwup,"swdn_clr":swdn_clr,"swup_clr":swup_clr,"lwdn_clr":lwdn_clr,"lwup_clr":lwup_clr}
    return output