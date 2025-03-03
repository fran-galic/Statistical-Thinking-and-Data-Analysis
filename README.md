# **Project Description and Dataset Overview**

## **Project Description**
In this project, we analyzed student performance by examining various demographic, academic, and behavioral factors. Our goal was to identify patterns and relationships that influence academic success in key subjects such as mathematics and Portuguese. 

To achieve this, we explored multiple research questions:
- Are the average final grades in mathematics different between genders?
- Is there a difference in the first mathematics grade based on students' place of residence?
- Can we predict passing the final language exam based on socio-demographic variables such as gender, parental education, and family size?
- Is there a difference in the number of absences from classes?

By investigating these questions, we aimed to uncover insights that could help educators, parents, and students better understand the factors affecting academic performance.

Final reports from our analysis are stored in the `final_reports` directory.

---

## **Dataset Overview**
The dataset we worked with contains information about students' demographic characteristics, educational habits, and grades. It includes variables related to students' personal background, parental education, school choices, study habits, and academic performance.

---

### **Demographic Information**
- **school**: Student’s school (`'GP'` – Gabriel Pereira, `'MS'` – Mousinho da Silveira)  
- **sex**: Student’s gender (`'F'` – female, `'M'` – male)  
- **age**: Student’s age (ranging from `15` to `22` years)  
- **address**: Type of home address (`'U'` – urban, `'R'` – rural)  
- **famsize**: Family size (`'LE3'` – less than or equal to 3, `'GT3'` – greater than 3)  
- **Pstatus**: Parent's cohabitation status (`'T'` – living together, `'A'` – apart)  

---

### **Parental Education**
- **Medu**: Mother’s education level (`0` – no education, `1` – primary education (4th grade), `2` – 5th to 9th grade, `3` – secondary education, `4` – higher education)  
- **Fedu**: Father’s education level (`0` – no education, `1` – primary education (4th grade), `2` – 5th to 9th grade, `3` – secondary education, `4` – higher education)  

---

### **Parental Occupation**
- **Mjob**: Mother’s occupation (`'teacher'`, `'health'`, `'civil services'`, `'at home'`, `'other'`)  
- **Fjob**: Father’s occupation (`'teacher'`, `'health'`, `'civil services'`, `'at home'`, `'other'`)  

---

### **School Selection and Conditions**
- **reason**: Reason for choosing the school (`'close to home'`, `'school reputation'`, `'course preference'`, `'other'`)  
- **guardian**: Student’s guardian (`'mother'`, `'father'`, `'other'`)  

---

### **Time and Study Habits**
- **traveltime**: Travel time from home to school (`1` – less than 15 minutes, `2` – 15 to 30 minutes, `3` – 30 minutes to 1 hour, `4` – more than 1 hour)  
- **studytime**: Weekly study time (`1` – less than 2 hours, `2` – 2 to 5 hours, `3` – 5 to 10 hours, `4` – more than 10 hours)  
- **failures_mat**: Number of past failures in mathematics (`n` if `0 ≤ n ≤ 3`, otherwise `4`)  
- **failures_por**: Number of past failures in Portuguese (`n` if `0 ≤ n ≤ 3`, otherwise `4`)  

---

### **Support and Activities**
- **schoolsup**: Additional educational support (`'yes'`, `'no'`)  
- **famsup**: Family educational support (`'yes'`, `'no'`)  
- **paid_mat**: Extra paid classes in mathematics (`'yes'`, `'no'`)  
- **paid_por**: Extra paid classes in Portuguese (`'yes'`, `'no'`)  
- **activities**: Extracurricular activities (`'yes'`, `'no'`)  
- **nursery**: Attended nursery school (`'yes'`, `'no'`)  
- **higher**: Aspiration to pursue higher education (`'yes'`, `'no'`)  
- **internet**: Internet access at home (`'yes'`, `'no'`)  
- **romantic**: In a romantic relationship (`'yes'`, `'no'`)  

---

### **Personal and Social Well-being**
- **famrel**: Quality of family relationships (`1` – very bad to `5` – excellent)  
- **freetime**: Free time after school (`1` – very low to `5` – very high)  
- **goout**: Going out with friends (`1` – very low to `5` – very high)  

---

### **Behavior and Health**
- **Dalc**: Alcohol consumption on weekdays (`1` – very low to `5` – very high)  
- **Walc**: Alcohol consumption on weekends (`1` – very low to `5` – very high)  
- **health**: Current health status (`1` – very bad to `5` – very good)  

---

### **School Absences and Grades**
- **absences_mat**: Number of absences in mathematics (`0` to `93`)  
- **absences_por**: Number of absences in Portuguese (`0` to `93`)  
- **G1_mat**: First-period grade in mathematics (`0` to `20`)  
- **G2_mat**: Second-period grade in mathematics (`0` to `20`)  
- **G3_mat**: Final grade in mathematics (`0` to `20`)  
- **G1_por**: First-period grade in Portuguese (`0` to `20`)  
- **G2_por**: Second-period grade in Portuguese (`0` to `20`)  
- **G3_por**: Final grade in Portuguese (`0` to `20`)  
