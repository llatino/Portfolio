import requests
import pandas as pd
from gazpacho import Soup
import csv

max_page = int(input("ต้องการดึงข้อมูลทั้งหมดกี่หน้า: "))
book_list =[]
main_url = "https://books.toscrape.com/"


for page in range(1,max_page+1):

    print(f"{page} / {max_page}")
    outer_url = f"{main_url}/catalogue/page-{page}.html"

    resp = requests.get(outer_url)
    resp.encoding = 'utf-8'
    soup = Soup(resp.text)


    items = soup.find("h3")


    for name in range(len(items)):
        title = soup.find("h3")[name].find("a").attrs['title']
        price = soup.find("p", {"class": "price_color"})[name].text

        link = soup.find("h3")[name].find("a").attrs['href']
        inner_url = f"{main_url}catalogue/{link}"
    
        print(f"Loading: 1{inner_url}")

        inner_resp = requests.get(inner_url)
        inner_soup = Soup(inner_resp.text)

        category = inner_soup.find("ul",{"class": "breadcrumb"}).find("a")[2].text
        data = {
            "title": title,
            "price": price,
            "category": category,
        }
    
        book_list.append(data)


df_title = pd.DataFrame(book_list)
df_title.index = df_title.index + 1

df_title.to_csv("Book list.csv")





