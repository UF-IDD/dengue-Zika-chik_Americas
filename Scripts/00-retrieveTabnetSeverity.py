#!/usr/bin/env python
# -*- coding: utf-8 -*-


def main(args):
    # Code dependencies:
    # chromedriver, chrome web browser
    browser = webdriver.Chrome() #replace with .Firefox(), or with the browser of your choice
    browser.get(args.mainUrl) #navigate to the page

    if not os.path.exists(args.outdir):
        os.makedirs(args.outdir)

    # store all the years available
    years = browser.find_elements_by_xpath("//select[@name='Arquivos']/option")
    years = [ y.text for y in years ]

    # for each severity level
    for sev in ["severe","severealarm","total"]:
        # for each selectable year
        for y in years:
            yForm = Select(browser.find_elements_by_xpath("//select[@name='Arquivos']")[0])
            yForm.deselect_all()
            yForm.select_by_visible_text(y)
            y = y.rstrip('\n\r ')

            # choose row variable as: state
            browser.find_elements_by_xpath("//select[@name='Linha']/option[@value='UF_de_notificação']")[0].click()

            # choose column variable as: month
            browser.find_elements_by_xpath("//select[@name='Coluna']/option[@value='Mês_1º_Sintoma(s)']")[0].click()

            # choose severity filter
            browser.find_elements_by_xpath("//img[@id='"+args.figid+"']")[0].click()
            s = Select( browser.find_elements_by_xpath("//select[@name='SGrau_FHD']")[0] )
            s.deselect_all()
            if sev == "total":
                print 'Including all severities..'
            else:
                if sev == "severe":
                    s.select_by_visible_text("Grau I")
                    s.select_by_visible_text("Grau II")
                if sev == "severealarm":
                    s.select_by_visible_text("Grau III")
                    s.select_by_visible_text("Grau IV")

            # choose to display zeroes in results
            chk = browser.find_elements_by_xpath("//input[@name='zeradas']")[0]
            if not chk.is_selected():
                chk.click()

            # choose to get data with ';' as separator
            browser.find_elements_by_xpath("//input[@name='formato']")[2].click()

            # submit form
            browser.find_elements_by_xpath("//input[@name='mostre']")[0].click()

            # export query result to outdir
            t = ET.HTML(browser.page_source).xpath('//pre/text()')[0]
            outfile = args.outdir+'/'+sev+'_'+y+'.csv'

            with codecs.open(outfile,'w', encoding='utf-8') as out:
                out.write( t )

            browser.back()
    print 'Done scraping.'
    #browser.quit()




import os
import argparse
#import requests as req
from lxml import etree as ET
import codecs
from selenium import webdriver
from selenium.webdriver.support.ui import Select


if __name__=='__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("mainUrl")
    parser.add_argument("outdir")
    parser.add_argument("figid", help="fig number of the severity dropdown")
    main(parser.parse_args())
