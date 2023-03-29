def split_txt_luqv(file_path, save_path, save_name):
    num_dict = {
        '0', '1', '2', '3', '4', '5', '6','7', '8', '9',  
    }
    
    import pandas as pd
    index_list = []
    init_score_list = []
    fushi_score_list = []
    status_list = []
    
    content_list = []
    
    # split into multiple lines, then read line
    with open(file_path, 'r', encoding="utf-8") as f:
        content = f.readlines()
        for i in content:
            content_list = i.split("002全国统考无")
        f.close()

    for j in range(len(content_list)):
        if j == 0:      
            index_list.append(content_list[j][0:15])
            
        else:
            for i in range(len(content_list[j]), 0, -1):
                if i in num_dict == True:
                    continue
                else:
                    print(content_list[j])
                    print(content_list[j][i::-1])
                    index_list.append(content_list[j].rstrip(content_list[j][:i]))
                    break
        if j != 0:
            init_score_list.append(int(content_list[j][0:3]))
            fushi_score_list.append(int(content_list[j][3:6]))
            
        status_list.append(1)
    
    df = pd.DataFrame(list(zip(index_list, init_score_list, fushi_score_list, status_list)),
        columns =['index', 'init_score', 'fushi_score', 'status'])
    
    df.to_excel(save_path, sheet_name=str(save_name))
    
    return df
