import text_to_excel as te
import pandas as pd
import numpy as np

def admit_data_generate(excel_name, luqv_sheet_name, fushi_sheet_name, save_excel_name):
    import pandas as pd
    import numpy as np
    df_luqv_2022 = pd.read_excel(excel_name, sheet_name=luqv_sheet_name)
    df_fushi_2022 = pd.read_excel(excel_name, sheet_name=fushi_sheet_name)
    new_frame = {
        "考号": [],
        "初试分数": [],
        "复试分数":[],
        "录取状态":[],
    }
    df_2022 = pd.DataFrame(new_frame)
    index_luqv = df_luqv_2022["姓名"].to_numpy()
    
    for ind in df_fushi_2022.index:
        flag = False
        cunt = 0
        for i in index_luqv:
            if str(i) == str(df_fushi_2022["姓名"][ind]):
                index = cunt
                flag = True
                break
            else:
                flag = False
                cunt = cunt + 1
                
        if flag == True:
            #index = np.where(int(str(index_luqv[0:])[-4:]) == int(str(df_fushi_2022["考号"][ind])[-4:]))
            new_row = { "初试分数":df_fushi_2022["初试总分"][ind], "复试分数":df_luqv_2022["复试总分"][index], "录取状态":1}
            #'考号':df_fushi_2022["考号"][ind],
            
        else:
            new_row = { "初试分数":df_fushi_2022["初试总分"][ind], "复试分数":"" , "录取状态":0}
            #'考号':df_fushi_2022["考号"][ind],
        df_2022 = df_2022.append(new_row, ignore_index=True)
    
    df_2022.to_excel(save_excel_name)




if __name__ == "__main__":
    excel_name = "./data/天津大学.xlsx"
    save_name_2023 = "./data/天津大学2023.xlsx"
    save_name_2022 = "./data/天津大学2022.xlsx"
    save_name_2021 = "./data/天津大学2021.xlsx"
   
    admit_data_generate(excel_name, "2023拟录取", "2023复试",save_name_2023)
    #admit_data_generate(excel_name, "2022拟录取", "2022复试",save_name_2022)
    
    
